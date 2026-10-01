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
            func Single<T>(value: T) -> T[] {
                let values: T[] = [value]
                return values
            }
            func Repeat<T>(value: T, count: int) -> T {
                if count == 0 { return value }
                return Repeat<T>(value, count - 1)
            }
            func Last<T>(values: T[]) -> T {
                var last = values[0]
                for value in values { last = value }
                return last
            }
            func Choose<T>(flag: bool, first: T, second: T) -> T => if flag { first } else { second }
            func Select<T>(value: T) -> T => value
            func Select<T, U>(value: T, ignored: U) -> T => value
            class Receiver {
                private var number: int = 0
                func Remember<T>(value: T, next: int) -> T {
                    number = next
                    return value
                }
                func Forward<U>(value: U, next: int) -> U => Remember<U>(value, next)
                func Copy<T>(source: T[], destination: T[]) {
                    var index = 0
                    for value in source {
                        destination[index] = value
                        index = index + 1
                    }
                    number = index
                }
                func Clear<T>(values: T[]) {
                    var index = 0
                    while index < values.Length {
                        values[index] = default(T)
                        index = index + 1
                    }
                }
                func Empty<T>() -> T => default(T)
                func Reverse<T>(values: T[]) {
                    var left = 0
                    var right = values.Length - 1
                    while left < right {
                        let value = values[left]
                        values[left] = values[right]
                        values[right] = value
                        left = left + 1
                        right = right - 1
                    }
                }
                func Recur<T>(value: T, depth: int) -> T {
                    if depth == 0 { return Remember(value, 42) }
                    return Recur(value, depth - 1)
                }
                val Number: int => number
            }
            class Evaluation {
                private val receiver: Receiver = Receiver()
                private val source: Order[] = [Order(10, true), Order(32, false)]
                private val destination: Order[] = [Order(0, false), Order(0, false)]
                private var trace: int = 0
                func ReceiverValue() -> Receiver {
                    trace = trace * 10 + 1
                    return receiver
                }
                func Source() -> Order[] {
                    trace = trace * 10 + 2
                    return source
                }
                func Destination() -> Order[] {
                    trace = trace * 10 + 3
                    return destination
                }
                val Trace: int => trace
                val Total: int => destination[0].Number + destination[1].Number
            }
            class Helpers {
                static func First<T>(values: T[]) -> T => values[0]
            }
            func Main() -> int {
                if Forward<long>(5000000000L) != 5000000000L { return 1 }
                if !Forward<bool>(true) { return 2 }
                let values: Order[] = [Order(41, true)]
                let alias = Repeat(Forward(Helpers.First<Order>(values)), 3)
                let copies = Single(alias)
                copies[0].Number = 40
                if values[0].Number != 40 { return 3 }
                let wide: long[] = [1L, 5000000000L]
                if Last(wide) != 5000000000L { return 4 }
                let selected = Select<Order, long>(Select(alias), 1L)
                selected.Number = 41
                if values[0].Number != 41 { return 5 }
                let receiver = Receiver()
                let picked = receiver.Forward(Choose(false, Order(1, false), alias), 7)
                if receiver.Number != 7 { return 6 }
                let evaluation = Evaluation()
                evaluation.ReceiverValue().Copy(evaluation.Source(), evaluation.Destination())
                if evaluation.Trace != 123 || evaluation.Total != 42 { return 7 }
                let batch: Order[] = [Order(10, false), picked]
                receiver.Reverse(batch)
                batch[0].Number = 42
                if picked.Number != 42 || batch[1].Number != 10 { return 8 }
                let numbers: int[] = [1, 2, 3]
                receiver.Clear(numbers)
                if numbers[0] + numbers[1] + numbers[2] != 0 { return 10 }
                if receiver.Empty<bool>() { return 11 }
                if receiver.Empty<long>() != 0L { return 12 }
                let spare = receiver.Empty<Order>()
                let other = Receiver()
                if other.Recur(42, 3) != 42 || other.Number != 42 || receiver.Number != 7 { return 9 }
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
        var faultSource = consumer.Replace("return Identity<int>(values[0].Number)", "receiver.Clear(values)\nreturn values[0].Number");
        var fault = Compilation.Create("ClearedReference", [SyntaxTree.ParseText(order), SyntaxTree.ParseText(faultSource)], references,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        using (var native = new MemoryStream())
        {
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(fault, native, new(new("ClearedReference", new Version(1, 0, 0, 0)), core, []));
            if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
            var path = Path.Combine(output, "ClearedReference.dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (command == "verify" ? process.ExitCode != 0 : process.ExitCode == 0 || !text.Contains("NullReference"))
                    throw new Exception("cleared reference contract: " + text);
            }
            using var cli = new MemoryStream();
            var result = fault.Emit(cli);
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            try { Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null); throw new Exception("missing cleared-reference fault"); }
            catch (TargetInvocationException e) when (e.InnerException is NullReferenceException) { }
        }
        var rejectedContracts = 0;
        foreach (var unsupported in new[] {
            "class Box<T> { }",
            "open class Instance { virtual func Identity<T>(value: T) -> T => value }",
            "func Restricted<T>(value: T) -> T where T: class => value",
            "func Marker<T>() -> int => 42\nfunc Use() -> int => Marker<System.DateTime>()",
            "func Identity<T>(value: T) -> T => value\nfunc Use(values: int[][]) -> int[][] => Identity(values)"
        })
        {
            var compilation = Compilation.Create("UnsupportedGeneric", [SyntaxTree.ParseText(unsupported, path: "Unsupported.rvn")], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception("rejection fixture must bind: " + string.Join("; ", errors.Select(d => d.ToString())));
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("UnsupportedGeneric", new Version(1, 0, 0, 0)), core, []));
            if (result.Success || image.Length != 0 || !result.Diagnostics.Any(d => d.Id == "NEOMETA001" && d.Location.IsInSource))
                throw new Exception("unsupported generic requires source diagnostic and no output: " + string.Join("; ", result.Diagnostics));
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
            genericFunctions = true,
            genericStaticMethods = true,
            forwardedParameters = true,
            genericLocals = true,
            genericArrayAccess = true,
            objectAliasing = true,
            inferredCalls = true,
            recursiveGenericCalls = true,
            genericArrayCreation = true,
            genericArrayIteration = true,
            multipleParametersAndOverloads = true,
            genericConditionalValues = true,
            genericInstanceMethods = true,
            genericReceiverMutation = true,
            genericNoResultMethods = true,
            genericArrayCopyAndReverse = true,
            receiverArgumentOrder = true,
            recursiveGenericInstanceCalls = true,
            independentReceivers = true,
            genericDefaultValues = true,
            genericArrayClear = true,
            clearedReferenceFault = true,
            rejectedContracts

        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS generic Order consumer on CLI/native in both source orders: 42");
    }
}
