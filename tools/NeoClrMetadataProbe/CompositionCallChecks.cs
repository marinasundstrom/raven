using System.Text;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class CompositionCallChecks
{
    internal static void Run(string corePath, string output)
    {
        RunCase(corePath, output, false);
        RunCase(corePath, Path.Combine(output, "generic"), true);
        WrapperCompositionCallChecks.Run(corePath, Path.Combine(output, "wrappers"));
    }
    private static void RunCase(string corePath, string output, bool generic)
    {
        var bytes = File.ReadAllBytes(corePath);
        var coreDefinition = AssemblyDefinition.ReadAssembly(bytes, expectedExtended: false);
        var core = coreDefinition.Identity;
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var provider = new AssemblyBuilder(new("CompositionPocProvider", new(1, 0, 0, 0)), core);
        var aDefinition = generic ? provider.AddGenericInterface("Example", "A", ["T"]) : provider.AddInterface("Example", "A");
        var bDefinition = generic ? provider.AddGenericInterface("Example", "B", ["T"]) : provider.AddInterface("Example", "B");
        SignatureType a = generic ? aDefinition.MakeGenericInstance(PrimitiveType.Int32) : aDefinition;
        SignatureType b = generic ? bDefinition.MakeGenericInstance(PrimitiveType.Int32) : bDefinition;
        foreach (var name in new[] { "First", "Second" })
        {
            var type = provider.AddClass("Example", name);
            type.SetSealedClass();
            if (generic)
            {
                type.AddInterfaceImplementation(a.GenericInstance!);
                type.AddInterfaceImplementation(b.GenericInstance!);
            }
            else
            {
                type.AddInterfaceImplementation(aDefinition);
                type.AddInterfaceImplementation(bDefinition);
            }
            var ctor = type.AddConstructor(Array.Empty<PrimitiveType>()); ctor.Return();
            var make = provider.AddFunction("Make" + name, new MethodSignature(a, Array.Empty<SignatureType>()));
            make.NewObject(ctor); make.Return();
        }
        foreach (var name in new[] { "AsIntersection", "AsUnion" })
        {
            var method = provider.AddFunction(name, new MethodSignature(a, new SignatureType[] { a }));
            method.LoadArgument(0); method.Return();
        }
        foreach (var name in new[] { "Consume", "NeedBoth" })
        {
            var method = provider.AddFunction(name, new MethodSignature(PrimitiveType.Int32, new SignatureType[] { a }));
            method.LoadConstant(42); method.Return();
        }
        if (generic)
        {
            var wrong = provider.AddFunction("NeedWrong", new MethodSignature(PrimitiveType.Int32, new SignatureType[] { a }));
            wrong.LoadConstant(42); wrong.Return();
            provider.AddGenericClass("Example", "SealedBox", ["T"]).SetSealedClass();
        }
        var root = JsonNode.Parse(provider.WriteNativeAssembly())!;
        JsonNode Shape(string kind, params int[] indices) => new JsonObject
        {
            [kind] = new JsonArray(indices.Select(i => generic && (i < 2 || i == 4)
                ? (JsonNode)new JsonObject { ["Constructed"] = new JsonObject { ["definition"] = root["types"]![i]!["name"]!.DeepClone(), ["arguments"] = new JsonArray("Int32") } }
                : new JsonObject { ["Named"] = root["types"]![i]!["name"]!.DeepClone() }).ToArray())
        };
        var functions = root["functions"]!;
        functions[0]!["returns"] = Shape("Union", 2, 3);
        functions[1]!["returns"] = Shape("Union", 3, 2);
        functions[2]!["parameters"]![0] = Shape("Union", 2, 3);
        functions[2]!["returns"] = Shape("Intersection", 0, 1);
        functions[3]!["parameters"]![0] = Shape("Intersection", 1, 0);
        functions[3]!["returns"] = Shape("Union", 0, 1);
        functions[4]!["parameters"]![0] = Shape("Union", 1, 0);
        functions[5]!["parameters"]![0] = Shape("Intersection", 0, 1);
        if (generic)
        {
            var wrong = Shape("Intersection", 0, 1);
            wrong["Intersection"]![0]!["Constructed"]!["arguments"]![0] = "Boolean";
            functions[6]!["parameters"]![0] = wrong;
            byte[] Encode(JsonNode node) => NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(node.ToJsonString()));
            foreach (var argument in new JsonNode[] { JsonValue.Create("Void")!, new JsonObject { ["TypeParameter"] = 0 }, Shape("Union", 2, 3) })
            {
                var invalid = root.DeepClone();
                invalid["functions"]![2]!["returns"]!["Intersection"]![0]!["Constructed"]!["arguments"]![0] = argument.DeepClone();
                Reject(() => AssemblyDefinition.ReadNativeAssembly(Encode(invalid), true));
            }
            var wrongArity = root.DeepClone();
            wrongArity["functions"]![2]!["returns"]!["Intersection"]![0]!["Constructed"]!["arguments"] = new JsonArray();
            Reject(() => AssemblyDefinition.ReadNativeAssembly(Encode(wrongArity), true));
            foreach (var kind in new[] { "Union", "Intersection" })
            {
                var sealedBound = root.DeepClone();
                sealedBound["functions"]![2]!["returns"] = Shape(kind, 4, 4);
                if (kind == "Intersection") Reject(() => AssemblyDefinition.ReadNativeAssembly(Encode(sealedBound), true));
                else _ = AssemblyDefinition.ReadNativeAssembly(Encode(sealedBound), true);
            }
            var snapshot = AssemblyDefinition.ReadNativeAssembly(Encode(root), true);
            var host = new AssemblyBuilder(new("WrongGenericComposition", new(1, 0, 0, 0)), core);
            var main = host.AddFunction("Main"); host.EntryPoint = main;
            main.Call(host.ImportExperimentalCompositionReference(snapshot.MainModule.Functions.Single(m => m.Name == "MakeFirst"), core));
            main.Call(host.ImportExperimentalCompositionReference(snapshot.MainModule.Functions.Single(m => m.Name == "NeedWrong"), core));
            main.Return();
            Reject(() => host.WriteNativeAssembly());
        }
        var image = NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(root.ToJsonString()));
        Reject(() => NeoClrMetadataReference.ReadAssembly(image, bootstrap));
        Reject(() => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(image, bootstrap));
        var reference = NeoClrMetadataReference.ReadExperimentalCompositionAssembly(image, bootstrap);
        var options = CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithRuntimeTypeOfContract(null);
        Compilation Create(string name, string source) => Compilation.Create(name, [SyntaxTree.ParseText(source)], [bootstrap.Reference, reference], options);
        NeoClrEmitOptions EmitOptions(string name, bool enabled = true, bool oldOption = false) => new(new(name, new(1, 0, 0, 0)), core,
            [new(bootstrap.Reference, coreDefinition, core), new(reference, core)],
            allowExperimentalIntersectionCalls: oldOption, allowExperimentalCompositionCalls: enabled);
        Directory.CreateDirectory(output);
        File.WriteAllBytes(Path.Combine(output, "CompositionPocProvider.neox"), image);
        foreach (var name in new[] { "First", "Second", "Direct", "Discard", "Local" })
        {
            var source = name switch
            {
                "Local" => "func Main() -> int { let value = MakeFirst(); let copy = value; let both = AsIntersection(copy); return Consume(AsUnion(both)) }",
                "Direct" => "func Main() -> int => NeedBoth(MakeFirst())",
                "Discard" => "func Main() -> int { AsUnion(AsIntersection(MakeSecond())); return 42 }",
                _ => $"func Main() -> int => Consume(AsUnion(AsIntersection(Make{name}())))"
            };
            var compilation = Create(name, source);
            Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", compilation.GetDiagnostics()));
            using var stream = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.Emit(compilation, stream, EmitOptions(name));
            Check(emitted.Success, string.Join("; ", emitted.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, name + ".neox"), NativeModuleContainer.WriteLibraryBinary(stream.ToArray()));
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source + "\n");
            using var disabled = new MemoryStream();
            Check(!NeoClrCompilationEmitter.Emit(compilation, disabled, EmitOptions(name, false)).Success && disabled.Length == 0, "default gate");
            Check(!NeoClrCompilationEmitter.Emit(compilation, disabled, EmitOptions(name, false, true)).Success && disabled.Length == 0, "old intersection opt-in does not admit union references");
            Check(!NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, disabled, EmitOptions(name)).Success && disabled.Length == 0, "PE gate");
        }
        foreach (var source in new[] {
            "func Main() -> int => NeedBoth(AsUnion(AsIntersection(MakeFirst())))",
            "func Main() -> int { let value = AsUnion(AsIntersection(MakeFirst())); return NeedBoth(value) }" })
        {
            var compilation = Create("Rejected", source);
            using var stream = new MemoryStream();
            var result = NeoClrCompilationEmitter.Emit(compilation, stream, EmitOptions("Rejected"));
            Check(!result.Success && stream.Length == 0, "unsafe narrowing/storage rejection");
        }
        if (generic)
        {
            var mismatch = Create("WrongArgument", "func Main() -> int => NeedWrong(MakeFirst())");
            using var stream = new MemoryStream();
            Check(!NeoClrCompilationEmitter.Emit(mismatch, stream, EmitOptions("WrongArgument")).Success && stream.Length == 0,
                "generic argument mismatch rejects before output");
        }
        var semantic = Create("Semantic", "func Main() -> int => NeedBoth(MakeFirst())");
        _ = semantic.GetDiagnostics();
        var assembly = (IAssemblySymbol)semantic.GetAssemblyOrModuleSymbol(reference)!;
        var first = assembly.GlobalNamespace.GetMembers("MakeFirst").OfType<IMethodSymbol>().Single().ReturnType;
        var second = assembly.GlobalNamespace.GetMembers("MakeSecond").OfType<IMethodSymbol>().Single().ReturnType;
        Check(first is IStructuralUnionTypeSymbol && SymbolEqualityComparer.Default.Equals(first, second), "native union has structural unordered identity");
        Check(SymbolEqualityComparer.Default.GetHashCode(first) == SymbolEqualityComparer.Default.GetHashCode(second), "unordered hash");
        Check(first.ToDisplayString().Contains(" | "), "structural display");
        Console.WriteLine("PASS structural composition POC: both union alternatives, intersection forwarding, direct conversion, discard, unsafe narrowing and default/PE/storage gates");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
    private static void Reject(Action action)
    {
        try { action(); } catch (InvalidDataException) { return; }
        throw new Exception("expected reader rejection");
    }
}
