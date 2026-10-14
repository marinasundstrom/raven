using System.Text;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class IntersectionCallChecks
{
    internal static void Run(string corePath, string output)
    {
        var bytes = File.ReadAllBytes(corePath);
        var coreDefinition = AssemblyDefinition.ReadAssembly(bytes, expectedExtended: false);
        var core = coreDefinition.Identity;
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var provider = new AssemblyBuilder(new("IntersectionCalls", new(1, 0, 0, 0)), core);
        var a = provider.AddInterface("Example", "A");
        var b = provider.AddInterface("Example", "B");
        var both = provider.AddClass("Example", "Both");
        both.AddInterfaceImplementation(a); both.AddInterfaceImplementation(b);
        var ctor = both.AddConstructor(Array.Empty<PrimitiveType>()); ctor.Return();
        var factory = provider.AddFunction("Produce", new MethodSignature(a, Array.Empty<SignatureType>()));
        factory.NewObject(ctor); factory.Return();
        var echo = provider.AddFunction("Echo", new MethodSignature(a, new SignatureType[] { a }));
        echo.LoadArgument(0); echo.Return();
        var consume = provider.AddFunction("Consume", new MethodSignature(PrimitiveType.Int32, new SignatureType[] { a }));
        consume.LoadConstant(42); consume.Return();
        var alternate = provider.AddFunction("Alternate", new MethodSignature(a, Array.Empty<SignatureType>()));
        alternate.NewObject(ctor); alternate.Return();
        var root = JsonNode.Parse(provider.WriteNativeAssembly())!;
        JsonNode Named(int index) => new JsonObject { ["Named"] = root["types"]![index]!["name"]!.DeepClone() };
        JsonNode Compound(int second, bool reverse = false) => new JsonObject { ["Intersection"] = new JsonArray(Named(reverse ? second : 0), Named(reverse ? 0 : second)) };
        Directory.CreateDirectory(output);
        foreach (var reduced in new[] { false, true })
        {
            var second = reduced ? 2 : 1;
            var native = root.DeepClone();
            native["functions"]![0]!["returns"] = Compound(second);
            native["functions"]![1]!["parameters"]![0] = Compound(second, true);
            native["functions"]![1]!["returns"] = Compound(second);
            native["functions"]![2]!["parameters"]![0] = Compound(second, true);
            native["functions"]![3]!["returns"] = reduced
                ? new JsonObject { ["Intersection"] = new JsonArray(Named(1), Named(2)) }
                : Compound(second);
            var image = NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(native.ToJsonString()));
            var reference = NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(image, bootstrap);
            var name = reduced ? "ReducedCalls" : "IntersectionCallsConsumer";
            var options = CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithRuntimeTypeOfContract(null);
            Compilation Create(string source) => Compilation.Create(name, [SyntaxTree.ParseText(source)], [bootstrap.Reference, reference], options);
            NeoClrEmitOptions EmitOptions(bool enabled) => new(new(name, new(1, 0, 0, 0)), core,
                [new(bootstrap.Reference, coreDefinition, core), reduced ? new(reference, core) : new(reference, reference.Definition, core)], allowExperimentalIntersectionCalls: enabled);
            var consumer = Create("func Main() -> int => Consume(Echo(Produce()))");
            Check(!consumer.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", consumer.GetDiagnostics()));
            using var disabled = new MemoryStream();
            var disabledResult = NeoClrCompilationEmitter.Emit(consumer, disabled, EmitOptions(false));
            Check(!disabledResult.Success && disabledResult.Diagnostics.Any(d => d.Id == "NEOMETA001") && disabled.Length == 0, "default gate");
            using var stream = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.Emit(consumer, stream, EmitOptions(true));
            Check(emitted.Success, string.Join("; ", emitted.Diagnostics));
            var json = JsonNode.Parse(stream.ToArray())!;
            var descriptors = json["functions"]!.AsArray().SelectMany(f => f!["body"]!.AsArray())
                .Where(i => i!["op"]!.GetValue<string>() == "call" && i["arg"]?["parameters"]?.AsArray().Count == 1)
                .Select(i => i!["arg"]!["parameters"]![0]).ToArray();
            Check(descriptors.Length == 2 && descriptors.All(t => t?["Intersection"]?.AsArray().Count == 2), "raw descriptors survive semantic normalization");
            File.WriteAllBytes(Path.Combine(output, name + ".neox"), NativeModuleContainer.WriteLibraryBinary(stream.ToArray()));
            File.WriteAllBytes(Path.Combine(output, name + "Provider.neox"), image);
            if (reduced)
            {
                var mismatched = Create("func Main() -> int => Consume(Alternate())");
                Check(!mismatched.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "both native contracts normalize to the same nominal compiler type");
                using var mismatchOutput = new MemoryStream();
                var mismatch = NeoClrCompilationEmitter.Emit(mismatched, mismatchOutput, EmitOptions(true));
                Check(!mismatch.Success && mismatch.Diagnostics.Any(d => d.Id == "NEOMETA003") && mismatchOutput.Length == 0, "different raw contracts cannot be silently erased");
            }
            using var projected = new MemoryStream();
            var projection = NeoClrCompilationEmitter.EmitMetadataAssembly(consumer, projected, EmitOptions(true));
            Check(!projection.Success && projection.Diagnostics.Any(d => d.Id == "NEOMETA001") && projected.Length == 0, "PE projection stays gated");
            using var discarded = new MemoryStream();
            var discard = NeoClrCompilationEmitter.Emit(Create("func Main() -> int { Echo(Produce()); return 42 }"), discarded, EmitOptions(true));
            Check(discard.Success, string.Join("; ", discard.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, name + "Discard.neox"), NativeModuleContainer.WriteLibraryBinary(discarded.ToArray()));
            using var storage = new MemoryStream();
            var stored = NeoClrCompilationEmitter.Emit(Create("func Main() -> int { let value = Produce(); return Consume(value) }"), storage, EmitOptions(true));
            Check(!stored.Success && stored.Diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error) && storage.Length == 0, "authored storage stays gated even after reduction");
        }
        Console.WriteLine("PASS experimental native intersection calls: default gate, exact descriptors, reduced bounds, discarded results and storage/PE rejection");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}
