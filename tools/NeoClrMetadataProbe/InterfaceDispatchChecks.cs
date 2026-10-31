using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class InterfaceDispatchChecks
{
    internal const string Contracts = """
        public interface Value {
            func Get(offset: int) -> int
            val Current: int { get }
        }
        public interface Derived : Value { }
        """;
    internal const string Consumer = """
        public class First : Derived {
            func Get(offset: int) -> int => 19 + offset
            val Current: int => 19
        }
        public class Second : Derived {
            func Get(offset: int) -> int => 23 + offset
            val Current: int => 23
        }
        func Apply(value: Value) -> int => value.Get(1) + value.Current - 1
        func Main() -> int {
            let values: Value[] = [First(), Second()]
            let first = Apply(values[0])
            values[0] = values[1]
            return (first + Apply(values[0])) / 2
        }
        """;
    internal static async Task Run(string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh"); Directory.CreateDirectory(output);
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        foreach (var reverse in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(Contracts, path: "Contracts.rvn"), SyntaxTree.ParseText(Consumer, path: "Consumer.rvn") };
            if (reverse) Array.Reverse(trees);
            var name = "Dispatch" + (reverse ? "Reverse" : "Forward");
            var compilation = Compilation.Create(name, trees, [MetadataReference.CreateFromFile(typeof(object).Assembly.Location)], new CompilationOptions(OutputKind.ConsoleApplication));
            var diagnostics = compilation.GetDiagnostics();
            if (diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error)) throw new Exception(string.Join("; ", diagnostics));
            using var native = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
            var path = Path.Combine(output, name + ".dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!; var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(command + ": " + text);
            }
            using var cli = new MemoryStream(); var result = compilation.Emit(cli);
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            var loaded = Assembly.Load(cli.ToArray());
            if (!Equals(loaded.EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI dispatch result");
            var projection = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            if (projection.MainModule.Types.Single(t => t.Name == "First").Methods.Single(m => m.Name == "Get").IsStatic)
                throw new Exception("projected instance implementation");
        }
        File.WriteAllText(Path.Combine(output, "Contracts.rvn"), Contracts); File.WriteAllText(Path.Combine(output, "Consumer.rvn"), Consumer);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            contractsSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(Contracts))),
            consumerSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(Consumer))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            sourceOrders = new[] { "forward", "reverse" },
            cliResult = 42,
            nativeResult = 42,
            nativeVerified = true,
            interfaceDispatch = true,
            inheritedContract = true,
            propertyDispatch = true,
            referenceArray = true,
            limitations = "Owned nongeneric interfaces and implicit public root-class implementations; host-core bootstrap. Generic interface dispatch and explicit/default implementations remain unsupported by this adapter."
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS Raven CLI/native interface method/property dispatch to two implementations, both source orders: 42");
    }
}
