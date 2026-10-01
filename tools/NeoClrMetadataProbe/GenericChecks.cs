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

internal static class GenericChecks
{
    internal static async Task Run(string sourcePath, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var original = File.ReadAllText(sourcePath);
        var order = SyntaxTree.ParseText(original).GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Order").ToFullString();
        const string consumer = """
            func Identity<T>(value: T) -> T {
                let copy = value
                return copy
            }
            func Forward<U>(value: U) -> U => Identity<U>(value)
            class Helpers {
                static func First<T>(values: T[]) -> T => values[0]
            }
            func Main() -> int {
                if Forward<long>(5000000000L) != 5000000000L { return 1 }
                if !Forward<bool>(true) { return 2 }
                let values: Order[] = [Order(41, true)]
                let alias = Forward<Order>(Helpers.First<Order>(values))
                alias.Number = 42
                return Identity<int>(values[0].Number)
            }
            """;
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        foreach (bool reversed in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(order, path: "Order.rvn"), SyntaxTree.ParseText(consumer, path: "Main.rvn") };
            if (reversed) Array.Reverse(trees);
            var name = "OrderGeneric" + reversed;
            var compilation = Compilation.Create(name, trees, references, new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var snapshot = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            if (!snapshot.MainModule.Types.SelectMany(t => t.Methods).Any(m => m.GenericArity == 1))
                throw new Exception("generic reference projection missing");
            var path = Path.Combine(output, name + ".dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(text);
            }
            using var cli = new MemoryStream();
            var emitted = compilation.Emit(cli);
            if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
            if (!Equals(Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI generic result mismatch");
        }
        File.WriteAllText(Path.Combine(output, "Order.rvn"), order);
        File.WriteAllText(Path.Combine(output, "Main.rvn"), consumer);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            source = Path.GetFileName(sourcePath),
            sourceSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(original))),
            selectedSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(order))),
            consumerSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(consumer))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            sourceOrders = 2,
            cliResult = 42,
            nativeResult = 42,
            nativeVerify = true,
            fullConsumer = false,
            genericFunctions = true,
            genericStaticMethods = true,
            forwardedParameters = true,
            genericLocals = true,
            genericArrayAccess = true,
            objectAliasing = true

        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS generic Order consumer on CLI/native in both source orders: 42");
    }
}
