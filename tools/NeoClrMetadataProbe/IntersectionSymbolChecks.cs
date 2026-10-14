using System.Text;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class IntersectionSymbolChecks
{
    internal static void Run(string corePath)
    {
        var coreBytes = File.ReadAllBytes(corePath);
        var coreDefinition = AssemblyDefinition.ReadAssembly(coreBytes, expectedExtended: false);
        var core = coreDefinition.Identity;
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(coreBytes);
        var builder = new AssemblyBuilder(new("IntersectionLibrary", new(1, 0, 0, 0)), core);
        var a = builder.AddInterface("Example", "A");
        var b = builder.AddInterface("Example", "B");
        var derived = builder.AddInterface("Example", "Derived");
        derived.AddBaseInterface(a);
        var echo = builder.AddFunction("Echo", new MethodSignature(a, new SignatureType[] { a }));
        echo.LoadArgument(0); echo.Return();
        var produce = builder.AddFunction("Produce", new MethodSignature(a, Array.Empty<SignatureType>()));
        produce.LoadDefault(a); produce.Return();
        var root = JsonNode.Parse(builder.WriteNativeAssembly())!;
        JsonNode Named(int index) => new JsonObject { ["Named"] = root["types"]![index]!["name"]!.DeepClone() };
        JsonNode Compose(string kind, params JsonNode[] bounds) => new JsonObject { [kind] = new JsonArray(bounds.Select(b => b.DeepClone()).ToArray()) };
        byte[] Image(JsonNode input, JsonNode result, bool library)
        {
            var changed = root.DeepClone();
            changed["functions"]![0]!["parameters"]![0] = input.DeepClone();
            changed["functions"]![0]!["returns"] = result.DeepClone();
            changed["functions"]![1]!["returns"] = result.DeepClone();
            var json = Encoding.UTF8.GetBytes(changed.ToJsonString());
            return library ? NativeModuleContainer.WriteLibraryBinary(json) : NativeModuleContainer.WriteBinary(json);
        }
        var ab = Compose("Intersection", Named(0), Named(1));
        var ba = Compose("Intersection", Named(1), Named(0));
        var options = CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name)
            .WithRuntimeTypeOfContract(null).WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        foreach (var library in new[] { false, true })
        {
            var image = Image(ab, ba, library);
            Reject(() => NeoClrMetadataReference.ReadAssembly(image));
            Reject(() => NeoClrMetadataReference.ReadAssembly(image, bootstrap));
            var reference = NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(image, bootstrap);
            foreach (var warm in new[] { false, true })
            {
                var tree = SyntaxTree.ParseText("func Main() -> unit { let value = Echo(Produce()) }");
                var consumer = Compilation.Create("IntersectionConsumer", [tree], [bootstrap.Reference, reference], options);
                if (warm) CheckDiagnostics(consumer);
                var model = consumer.GetSemanticModel(tree);
                var calls = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();
                var call = calls.Single(c => c.Expression.ToString() == "Echo");
                var method = model.GetSymbolInfo(call).Symbol as IMethodSymbol;
                Check(method is not null, "cold/warm callable lookup");
                Check(method!.ReturnType is IIntersectionTypeSymbol { ConstituentTypes.Length: 2 }, "intersection result");
                Check(ReferenceEquals(method.ReturnType, method.Parameters[0].Type), "reordered canonical bounds");
                Check(SymbolEqualityComparer.Default.Equals(model.GetTypeInfo(call).Type, method.ReturnType), "semantic expression type");
                var expected = consumer.CreateIntersectionTypeSymbol(consumer.GetTypeByMetadataName("Example.A")!, consumer.GetTypeByMetadataName("Example.B")!);
                Check(SymbolEqualityComparer.Default.Equals(expected, method.ReturnType), "compiler-owned intersection identity");
                CheckDiagnostics(consumer);
                using var output = new MemoryStream();
                var result = NeoClrCompilationEmitter.EmitMetadataAssembly(consumer, output, new(new("IntersectionConsumer", new(1, 0, 0, 0)), core,
                    [new(bootstrap.Reference, coreDefinition, core), new(reference, reference.Definition, core)]));
                Check(!result.Success && result.Diagnostics.Any(d => d.Id == "NEOMETA001") && output.Length == 0, "compound emission remains gated before output");
                using var rawOutput = new MemoryStream();
                rawOutput.WriteByte(42);
                var rawResult = NeoClrCompilationEmitter.Emit(consumer, rawOutput, new(new("IntersectionConsumer", new(1, 0, 0, 0)), core,
                    [new(bootstrap.Reference, coreDefinition, core), new(reference, reference.Definition, core)]));
                Check(!rawResult.Success && rawResult.Diagnostics.Any(d => d.Id == "NEOMETA001") && rawOutput.Position == 1 && rawOutput.ToArray().SequenceEqual(new byte[] { 42 }), "raw emission rejection preserves existing output");
            }
            var reducedImage = Image(Compose("Intersection", Named(0), Named(2)), Compose("Intersection", Named(2), Named(0)), library);
            var reduced = NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(reducedImage, bootstrap);
            var reducedTree = SyntaxTree.ParseText("func Main() -> unit { let value = Echo(Produce()) }");
            var reducedConsumer = Compilation.Create("Reduced", [reducedTree], [bootstrap.Reference, reduced], options);
            var reducedCall = reducedTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single(c => c.Expression.ToString() == "Echo");
            var reducedMethod = (IMethodSymbol)reducedConsumer.GetSemanticModel(reducedTree).GetSymbolInfo(reducedCall).Symbol!;
            Check(SymbolEqualityComparer.Default.Equals(reducedMethod.ReturnType, reducedConsumer.GetTypeByMetadataName("Example.Derived")), "semantic nominal-supertype reduction");
            Check(reduced.Definition.MainModule.Functions[0].TryGetSignature(out var raw) && raw!.ReturnType.Composition!.Constituents.Count == 2, "native descriptor retained independently");
            CheckDiagnostics(reducedConsumer);
            using var reducedOutput = new MemoryStream();
            var reducedEmit = NeoClrCompilationEmitter.EmitMetadataAssembly(reducedConsumer, reducedOutput, new(new("Reduced", new(1, 0, 0, 0)), core,
                [new(bootstrap.Reference, coreDefinition, core), new(reduced, reduced.Definition, core)]));
            Check(!reducedEmit.Success && reducedOutput.Length == 0, "normalized nominal symbol cannot bypass native descriptor emission gate");
            var invalid = Compilation.Create("Invalid", [SyntaxTree.ParseText("func CheckValue(value: Example.A) -> unit { let result = Echo(value) }")], [bootstrap.Reference, reference], options);
            Check(invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "one bound cannot satisfy an intersection parameter");
            var valid = Compilation.Create("Valid", [SyntaxTree.ParseText("func CheckValue(value: Example.A) -> unit { let result = Echo(Produce()); let projected: Example.A = result }")], [bootstrap.Reference, reference], options);
            CheckDiagnostics(valid);
            var union = Compose("Union", Named(0), Named(1));
            Reject(() => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(Image(union, ab, library), bootstrap));
            Reject(() => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(Image(ab, union, library), bootstrap));
            Reject(() => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(Image(ab, Compose("Intersection", Named(0), union), library), bootstrap));
        }
        Console.WriteLine("PASS native experimental intersection import: two schemas, cold/warm queries, canonical identity, nominal reduction, union rejection and emission gate");
    }

    private static void CheckDiagnostics(Compilation compilation) => Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", compilation.GetDiagnostics()));
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
    private static void Reject(Action action)
    {
        try { action(); }
        catch (InvalidDataException) { return; }
        throw new Exception("expected explicit rejection");
    }
}
