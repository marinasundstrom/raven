using System.Diagnostics;
using System.Reflection;
using System.Text.Json;
using System.Security.Cryptography;
using System.Text;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class OrderObjectChecks
{
    internal static async Task Run(string sourcePath, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var original = File.ReadAllText(sourcePath);
        var declaration = SyntaxTree.ParseText(original).GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.ValueText == "Order");
        if (declaration.Parent is not CompilationUnitSyntax) throw new InvalidDataException("Order namespace selection needs updating");
        var order = declaration.ToFullString();
        const string consumer = """
            class Counter {
                private var Number: int
                public func Read() -> int => self.Number
                init(number: int) { Number = number }
                public func Next() -> int {
                    let previous = Number
                    Number = Add(Number, 1)
                    return previous
                }
                private func Add(left: int, right: int) -> int => left + right
                public func Combine(left: int, right: int) -> int => left * 10 + right
                public func Reset(number: int) { Number = number }
                public func Increment(amount: int) -> int {
                    Number = Add(Number, amount)
                    return Number
                }
            }
            class Gauge {
                private var amount: int
                init(amount: int) {
                    self.amount = amount
                    Offset = 0
                }
                var Offset: int {
                    get => field
                    set => field = value + 1
                }
                val Doubled: int => amount * 2
                var Amount: int {
                    get { return self.amount }
                    set {
                        if value < 0 { amount = 0 } else { amount = value }
                    }
                }
                val Adjusted: int {
                    get => amount + 1
                    private set => amount = value - 1
                }
                func Reset(value: int) { Adjusted = value }
            }
            func Main() -> int {
                if !Order(1, true).Pending { return 1 }
                if Order(2, false).Pending { return 2 }
                if Order(-2147483647 - 1, false).Number != -2147483647 - 1 { return 3 }
                if Order(2147483647, true).Number != 2147483647 { return 4 }
                let original = Order(41, true)
                let alias = original
                alias.Number = 42
                alias.Pending = false
                if original.Pending { return 5 }
                let counter = Counter(1)
                if counter.Combine(counter.Next(), counter.Next()) != 12 { return 6 }
                if counter.Read() != 3 { return 7 }
                counter.Reset(40)
                if counter.Increment(2) != original.Number { return 8 }
                let gauge = Gauge(3)
                if gauge.Doubled != 6 { return 9 }
                gauge.Amount = -1
                if gauge.Amount != 0 { return 10 }
                gauge.Offset = 41
                if gauge.Offset != 42 { return 13 }
                gauge.Reset(22)
                if gauge.Adjusted != 22 { return 11 }
                if gauge.Doubled != original.Number { return 12 }
                return original.Number
            }
            """;
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        foreach (bool reversed in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(order, path: "Order.rvn"), SyntaxTree.ParseText(consumer, path: "Main.rvn") };
            if (reversed) Array.Reverse(trees);
            var name = "OrderObject" + reversed;
            var compilation = Compilation.Create(name, trees, references, new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var snapshot = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            var type = snapshot.MainModule.Types.Single(t => t.Name == "Order");
            if (type.Fields.Count != 2 || type.Properties.Count != 2 || type.Methods.Count != 5 || type.Properties.Any(p => p.GetMethod is null || p.SetMethod is null))
                throw new Exception("Order metadata lost members");
            var counterType = snapshot.MainModule.Types.Single(t => t.Name == "Counter");
            if (counterType.Fields.Count != 1 || counterType.Properties.Count != 0 || counterType.Methods.Count != 7)
                throw new Exception("private storage must emit only a field");
            var gaugeType = snapshot.MainModule.Types.Single(t => t.Name == "Gauge");
            if (gaugeType.Fields.Count != 2 || gaugeType.Properties.Count != 4 || gaugeType.Methods.Count != 9 ||
                gaugeType.Properties.Single(p => p.Name == "Doubled").SetMethod is not null ||
                (gaugeType.Properties.Single(p => p.Name == "Adjusted").SetMethod!.Attributes & (ushort)MethodAttributes.MemberAccessMask) != (ushort)MethodAttributes.Private)
                throw new Exception("computed accessor metadata mismatch");
            var path = Path.Combine(output, name + ".dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync();
                var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(text);
            }
            using var cli = new MemoryStream();
            var emitted = compilation.Emit(cli);
            if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
            if (!Equals(Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI result mismatch");
        }
        foreach (var unsupported in new[] {
            "class Empty { }",
            "class AccessorStorage { var Number: int { get; set; }\n init() { Number = 1 } }",
            "class PrivateInitialized { private var number: int = 1\n init() { } }",
            "class Initialized { var Number: int = 1\n init() { } }",
            order + "\nfunc Main() -> int { let order: Order? = null\n return 42 }"
        })
        {
            var compilation = Compilation.Create("UnsupportedObject", [SyntaxTree.ParseText(unsupported)], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception("rejection fixture must bind: " + string.Join("; ", errors.Select(d => d.ToString())));
            using var image = new MemoryStream();
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("UnsupportedObject", new Version(1, 0, 0, 0)), core, []));
            if (rejected.Success || image.Length != 0) throw new Exception("unsupported object contract wrote output");
        }
        File.WriteAllText(Path.Combine(output, "Order.rvn"), order);
        File.WriteAllText(Path.Combine(output, "Main.rvn"), consumer);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            source = Path.GetFileName(sourcePath),
            sourceSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(original))),
            consumerSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(consumer))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            selectedSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(order))),
            sourceOrders = 2,
            cliResult = 42,
            nativeResult = 42,
            nativeVerify = true,
            fullConsumer = false,
            nominalLocalsAndAliasing = true,
            ordinaryInstanceCalls = true,
            privatePrimitiveStorage = true,
            computedAndExplicitAccessors = true,
            rejectedIncompleteContracts = 5
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS unchanged Order constructor/properties -> .NET and binary neoCLR 42, both source orders");
    }
}
