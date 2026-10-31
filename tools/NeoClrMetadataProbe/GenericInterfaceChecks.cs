using System.Diagnostics;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class GenericInterfaceChecks
{
    internal const string Contracts = """
        public interface Echo<TTag> {
            func Apply<T>(value: T) -> T
        }
        public interface Derived : Echo<string> { }
        """;
    internal const string Consumer = """
        public class First : Derived {
            func Apply<T>(value: T) -> T => value
        }
        func Forward<T>(value: Echo<string>, state: T) -> T => value.Apply<T>(state)
        func Main() -> int {
            let echo: Echo<string> = First()
            let text = Forward<string>(echo, "hello")
            return Forward<int>(echo, 42)
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
                if (command == "run") { start.ArgumentList.Add("--execution"); start.ArgumentList.Add("interpreter"); }
                using var process = Process.Start(start)!; var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(command + ": " + text);
            }
        }
        File.WriteAllText(Path.Combine(output, "Contracts.rvn"), Contracts); File.WriteAllText(Path.Combine(output, "Consumer.rvn"), Consumer);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            contractsSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(Contracts))),
            consumerSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(Consumer))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            sourceOrders = new[] { "forward", "reverse" },
            nativeResult = 42,
            nativeVerified = true,
            interfaceDispatch = true,
            inheritedContract = true,
            genericMethodForwarding = true,
            stateTypes = new[] { "Int32", "String" },
            limitations = "Owned interfaces and implicit public root-class implementations; host-core bootstrap. Unconstrained generic instance interface methods; generic class virtual methods and constrained/static generic interface contracts remain outside this slice."
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS Raven native generic interface forwarding with Int32/String state, both source orders: 42");
    }
}
