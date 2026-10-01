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

internal static class ArrayChecks
{
    internal static async Task Run(string sourcePath, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var original = File.ReadAllText(sourcePath);
        var order = SyntaxTree.ParseText(original).GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Order").ToFullString();
        const string consumer = """
            func Identity(items: Order[]) -> Order[] => items
            func Identity(items: int[]) -> int[] => items
            class Batch {
                private val stored: Order[]
                var Items: Order[]
                init(items: Order[]) { stored = items; Items = items }
                func Read() -> Order[] => stored
            }
            class Counter {
                private var value: int = 0
                func Next() -> int { value = value + 1; return value }
                func Read() -> int => value
            }
            func Main() -> int {
                let batch: Order[] = [Order(101, true), Order(202, false), Order(303, true)]
                let holder = Batch(Identity(batch))
                let alias = holder.Read()
                alias[0].Number = 40
                if holder.Items[0].Number != 40 { return 1 }
                holder.Items[1] = Order(2, true)
                if batch[1].Number != 2 { return 2 }
                let empty: Order[] = []
                if empty.Length != 0 { return 3 }
                let counter = Counter()
                let numbers: int[] = [counter.Next(), counter.Next(), counter.Next()]
                Identity(numbers)[counter.Next() - 4] = counter.Next()
                if numbers[0] != 5 || numbers[1] != 2 || numbers[2] != 3 { return 4 }
                let wide: long[] = [5000000000L, -5000000000L]
                if wide[0] + wide[1] != 0L { return 5 }
                let flags: bool[] = [true, false]
                flags[1] = flags[0]
                if !flags[1] { return 6 }
                let text: string[] = ["array", "λ"]
                text[1] = text[0]
                if text.Length != 2 || batch.Length != 3 { return 7 }
                return batch[0].Number + batch[1].Number
            }
            """;
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        foreach (bool reversed in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(order, path: "Order.rvn"), SyntaxTree.ParseText(consumer, path: "Main.rvn") };
            if (reversed) Array.Reverse(trees);
            var name = "OrderArray" + reversed;
            var compilation = Compilation.Create(name, trees, references, new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var snapshot = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            var type = snapshot.MainModule.Types.Single(t => t.Name == "Batch");
            if (type.Fields.Count != 2 || type.Properties.Count != 1 || type.Properties[0].GetMethod is null || type.Properties[0].SetMethod is null ||
                !type.Fields.All(f => f.GetSignature()[1] == 0x1d) || type.Properties[0].GetSignature()[2] != 0x1d)
                throw new Exception("array storage projection lost signatures");
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
            if (!Equals(Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI array result mismatch");
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
            sourceOrders = 2, cliResult = 42, nativeResult = 42, nativeVerify = true,
            fullConsumer = false, arrays = true, orderedEvaluation = true, aliasing = true, storageProjection = true
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS Order arrays on CLI/native in both source orders: 42; signatures, storage, aliases, ordered evaluation");
    }
}
