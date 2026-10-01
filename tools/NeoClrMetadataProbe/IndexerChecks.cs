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

internal static class IndexerChecks
{
    internal static async Task Run(string sourcePath, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var original = File.ReadAllText(sourcePath);
        var order = SyntaxTree.ParseText(original).GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Order").ToFullString();
        const string consumer = """
            class Buffer {
                private val data: int[] = [1, 2, 3]
                var self[index: int]: int {
                    get => data[index]
                    set => data[index] = value
                }
                val self[index: long]: int => data[0]
                var self[row: int, column: int]: int {
                    get => data[row + column]
                    set => data[row + column] = value
                }
            }
            class OrderBuffer {
                private val data: Order[]
                init(items: Order[]) { data = items }
                var self[index: int]: Order {
                    get => data[index]
                    set => data[index] = value
                }
                val self[item: Order]: int => item.Number
                val self[indices: int[]]: Order => data[indices[0]]
                val Count: int => data.Length
            }
            class Evaluation {
                private val buffer: OrderBuffer
                private var trace: int = 0
                init(value: OrderBuffer) { buffer = value }
                func Receiver() -> OrderBuffer {
                    trace = trace * 10 + 1
                    return buffer
                }
                func Index() -> int {
                    trace = trace * 10 + 2
                    return 1
                }
                func Value() -> Order {
                    trace = trace * 10 + 3
                    return Order(40, true)
                }
                val Trace: int => trace
            }
            func Main() -> int {
                let buffer = Buffer()
                buffer[0, 1] = 40
                buffer[0] = 2
                if buffer[0L] != 2 { return 1 }
                if buffer[1] + buffer[0] != 42 { return 2 }
                let batch: Order[] = [Order(101, true), Order(202, false), Order(303, true)]
                let orders = OrderBuffer(batch)
                let evaluation = Evaluation(orders)
                evaluation.Receiver()[evaluation.Index()] = evaluation.Value()
                if evaluation.Trace != 123 || batch[1].Number != 40 { return 3 }
                let selected: int[] = [1]
                let alias = orders[selected]
                alias.Number = 41
                if orders[1].Number != 41 { return 4 }
                orders[1].Number = 42
                if orders[alias] != 42 || orders.Count != 3 { return 5 }
                var total = 0
                for order in batch {
                    if order.Pending && order.Number == 42 { total = total + order.Number }
                }
                return total
            }
            """;
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        foreach (bool reversed in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(order, path: "Order.rvn"), SyntaxTree.ParseText(consumer, path: "Main.rvn") };
            if (reversed) Array.Reverse(trees);
            var name = "OrderIndexer" + reversed;
            var compilation = Compilation.Create(name, trees, references, new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var snapshot = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            var type = snapshot.MainModule.Types.Single(t => t.Name == "Buffer");
            if (type.Properties.Count != 3 || type.Properties.Count(p => p.SetMethod is null) != 1 ||
                !type.Properties.Select(p => p.GetSignature()[1]).Order().SequenceEqual(new byte[] { 1, 1, 2 }))
                throw new Exception("indexed property projection lost parameters/accessors");
            var collection = snapshot.MainModule.Types.Single(t => t.Name == "OrderBuffer");
            if (collection.Properties.Count(p => p.Name == "Item") != 3 ||
                collection.Properties.Where(p => p.Name == "Item").Any(p => p.GetSignature()[1] != 1))
                throw new Exception("nominal/array index signatures lost");
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
        var faultCompilation = Compilation.Create("IndexerBounds",
            [SyntaxTree.ParseText(order), SyntaxTree.ParseText(consumer.Replace("return total", "return orders[9].Number"))], references,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        using (var native = new MemoryStream())
        {
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(faultCompilation, native,
                new(new("IndexerBounds", new Version(1, 0, 0, 0)), core, []));
            if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
            var path = Path.Combine(output, "IndexerBounds.dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (command == "verify" ? process.ExitCode != 0 : process.ExitCode == 0 || !text.Contains("IndexOutOfRange"))
                    throw new Exception("indexed bounds propagation: " + text);
            }
            using var cli = new MemoryStream();
            var result = faultCompilation.Emit(cli);
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            try { Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null); throw new Exception("missing CLI bounds fault"); }
            catch (TargetInvocationException e) when (e.InnerException is IndexOutOfRangeException) { }
        }
        var rejectedContracts = 0;
        foreach (var unsupported in new[] {
            "class FixedIndex { val self[index: int[2]]: int => 42 }",
            "class NestedIndex { val self[index: int[][]]: int => 42 }"
        })
        {
            var compilation = Compilation.Create("UnsupportedIndexer", [SyntaxTree.ParseText(unsupported)], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            if (compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error)) throw new Exception("rejection fixture must bind");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("UnsupportedIndexer", new Version(1, 0, 0, 0)), core, []));
            if (result.Success || image.Length != 0) throw new Exception("unsupported indexer produced output");
            rejectedContracts++;
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
            indexedProperties = true,
            overloads = true,
            multipleIndices = true,
            readonlyIndexer = true,
            nominalAndArrayIndices = true,
            objectAliasing = true,
            receiverIndexValueOrder = true,
            indexedBoundsFault = true,
            rejectedContracts
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS Order indexed collection on CLI/native in both source orders: 42; overloads, object aliases, ordered evaluation; unsupported signatures reject");
    }
}
