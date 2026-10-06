using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ScopeExitCleanupChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var identity = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(identity.Name!, identity.Version!, "", Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
        var resources = """
            namespace Contracts {
                public interface Propagatable<TSelf, TOutput, TResidual> {
                    func TryGetOutput(out value: TOutput) -> bool
                    func TryGetResidual(out value: TResidual) -> bool
                }
            }
            public interface ResourceProtocol { func Dispose() -> unit }
            public class Tracker { public var Value: int = 0 }
            public class Resource : ResourceProtocol {
                val Owner: Tracker
                val Id: int
                init(owner: Tracker, id: int) { Owner = owner; Id = id }
                func Dispose() -> unit { Owner.Value = Owner.Value * 10 + Id }
            }
            """;
        var cases = new (string Name, string Body, int Log, string ResultType, string Helpers, string ResultCheck)[]
        {
            ("Return", """
                use a = Resource(owner, 1)
                use b = Resource(owner, 2)
                return 42
                """, 21, "int", "", "return result"),
            ("Goto", """
                use a = Resource(owner, 1)
                if true {
                    use b = Resource(owner, 2)
                    goto done
                }
                done: return 42
                """, 21, "int", "", "return result"),
            ("Loop", """
                use a = Resource(owner, 1)
                var i = 0
                while i < 2 {
                    use b = Resource(owner, 2)
                    i = i + 1
                    if i == 1 { continue }
                    break
                }
                return 42
                """, 221, "int", "", "return result"),
            ("Value", """
                use a = Resource(owner, 1)
                let value = {
                    use b = Resource(owner, 2)
                    owner.Value = 9
                    42
                }
                return value
                """, 921, "int", "", "return result"),
            ("None", """
                use a = Resource(owner, 1)
                use b = Resource(owner, 2)
                let value = Read()?
                return Option()
                """, 21, "Option", """
                public struct Option : Contracts.Propagatable<Option, int, int> {
                    func TryGetOutput(out value: int) -> bool { value = 0; return false }
                    func TryGetResidual(out value: int) -> bool { value = 0; return true }
                    static func FromResidual(value: int) -> Option => Option()
                }
                func Read() -> Option => Option()
                """, "return 42"),
            ("Error", """
                use a = Resource(owner, 1)
                use b = Resource(owner, 2)
                let value = Read()?
                return Result(1)
                """, 21, "Result", """
                public struct Result : Contracts.Propagatable<Result, int, int> {
                    val Error: int
                    init(error: int) { Error = error }
                    func TryGetOutput(out value: int) -> bool { value = 0; return false }
                    func TryGetResidual(out value: int) -> bool { value = Error; return true }
                    static func FromResidual(value: int) -> Result => Result(value)
                }
                func Read() -> Result => Result(42)
                """, "return result.Error"),
        };
        var reports = new List<object>();
        foreach (var item in cases)
        {
            var name = "UseCleanup" + item.Name;
            var source = resources + "\n" + $$"""
                {{item.Helpers}}
                func Work(owner: Tracker) -> {{item.ResultType}} {
                    {{item.Body}}
                }
                func Main() -> int {
                    let owner = Tracker()
                    let result = Work(owner)
                    if owner.Value != {{item.Log}} { return 1 }
                    {{item.ResultCheck}}
                }
                """;
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            // An authored protocol avoids assuming the bootstrap facade is an
            // independently implementable native interface. This tests the shared
            // policy with the actual neoCLR target, not a CLR execution substitute.
            var options = CompilationOptions.NeoCLR.WithRuntimeDisposalContract(new(name, "ResourceProtocol", false))
                // Isolate cleanup from runtime library packaging while exercising
                // the explicit target propagation protocol (no exception capture).
                .WithRuntimePropagationContract(new(name, "Contracts.Propagatable`3"));
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)], [MetadataReference.CreateFromFile(corePath)], options);
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, image.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command);
                start.ArgumentList.Add(path);
                using var process = Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync();
                var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync();
                var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(command + ": " + text);
                reports.Add(new { item.Name, command, exitCode = process.ExitCode, output = text });
            }
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            coreSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(corePath))),
            scenarios = reports
        }, new JsonSerializerOptions { WriteIndented = true }));
        Console.WriteLine("Six neoCLR scope-exit cleanup consumers verified and executed (42).");
    }
}
