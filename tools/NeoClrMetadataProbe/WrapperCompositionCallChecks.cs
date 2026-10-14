using System.Text;
using System.Text.Json.Nodes;
using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class WrapperCompositionCallChecks
{
    internal static void Run(string corePath, string output)
    {
        var bytes = File.ReadAllBytes(corePath);
        var coreDefinition = AssemblyDefinition.ReadAssembly(bytes, expectedExtended: false);
        var core = coreDefinition.Identity;
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var provider = new AssemblyBuilder(new("WrapperProvider", new(1, 0, 0, 0)), core);
        var a = provider.AddInterface("Example", "A");
        var b = provider.AddInterface("Example", "B");
        var both = provider.AddClass("Example", "Both");
        both.AddInterfaceImplementation(a); both.AddInterfaceImplementation(b);
        var bothCtor = both.AddConstructor(Array.Empty<PrimitiveType>()); bothCtor.Return();
        var wrapper = provider.AddGenericClass("Example", "Wrapper", ["T"]);
        var value = wrapper.AddField("Value", SignatureType.TypeParameter(0));
        var ctor = wrapper.AddConstructor(new MethodSignature(PrimitiveType.Void, [SignatureType.TypeParameter(0)]));
        ctor.LoadArgument(0); ctor.LoadArgument(1); ctor.StoreField(value); ctor.Return();
        var get = wrapper.AddInstanceMethod("Get", new MethodSignature(SignatureType.TypeParameter(0), []));
        get.LoadArgument(0); get.LoadField(value); get.Return();
        var closed = wrapper.MakeGenericInstance(both);
        var make = provider.AddFunction("Make", new MethodSignature(closed, []));
        make.NewObject(bothCtor); make.NewObject(ctor.MakeConstructedReference([both])); make.Return();
        var echo = provider.AddFunction("Echo", new MethodSignature(closed, [closed]));
        echo.LoadArgument(0); echo.Return();
        var consume = provider.AddFunction("Consume", new MethodSignature(PrimitiveType.Int32, [closed]));
        consume.LoadArgument(0); consume.Call(get.MakeConstructedReference([both])); consume.Emit(OpCode.Pop); consume.LoadConstant(42); consume.Return();
        provider.AddGenericInterface("Example", "GenericInterface", ["T"]);
        provider.AddGenericValueType("Example", "GenericValue", ["T"]);
        var original = JsonNode.Parse(provider.WriteNativeAssembly())!;
        var wrapperName = original["types"]![3]!["name"]!.GetValue<string>();
        foreach (var kind in new[] { "Union", "Intersection" })
        {
            var root = original.DeepClone();
            JsonNode Shape(bool reversed = false) => new JsonObject { [kind] = new JsonArray(
                new JsonObject { ["Named"] = root["types"]![reversed ? 1 : 0]!["name"]!.DeepClone() },
                new JsonObject { ["Named"] = root["types"]![reversed ? 0 : 1]!["name"]!.DeepClone() }) };
            void Rewrite(JsonNode? node)
            {
                if (node is JsonObject obj)
                {
                    if (obj["owner"]?["Constructed"]?["definition"]?.GetValue<string>() == wrapperName && obj["parameters"] is JsonArray { Count: 1 } parameters)
                        parameters[0] = Shape();
                    if (obj["Constructed"] is JsonObject construction && construction["definition"]!.GetValue<string>() == wrapperName)
                        construction["arguments"] = new JsonArray(Shape());
                    foreach (var child in obj.ToArray()) Rewrite(child.Value);
                }
                else if (node is JsonArray array) foreach (var child in array) Rewrite(child);
            }
            foreach (var function in root["functions"]!.AsArray().Take(3)) Rewrite(function);
            root["functions"]![1]!["parameters"]![0]!["Constructed"]!["arguments"]![0] = Shape(true);
            byte[] Encode(JsonNode node) => NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(node.ToJsonString()));
            var wrongArity = root.DeepClone();
            wrongArity["functions"]![0]!["returns"]!["Constructed"]!["arguments"]!.AsArray().Add(Shape());
            Reject(() => AssemblyDefinition.ReadNativeAssembly(Encode(wrongArity), true));
            foreach (var index in new[] { 4, 5 })
            {
                var invalidWrapper = root.DeepClone();
                invalidWrapper["functions"]![0]!["returns"]!["Constructed"]!["definition"] = root["types"]![index]!["name"]!.DeepClone();
                Reject(() => AssemblyDefinition.ReadNativeAssembly(Encode(invalidWrapper), true));
            }
            var arrayWrapper = root.DeepClone();
            arrayWrapper["functions"]![0]!["returns"] = new JsonObject { ["ArrayRef"] = Shape() };
            Reject(() => AssemblyDefinition.ReadNativeAssembly(Encode(arrayWrapper), true));
            var image = NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(root.ToJsonString()));
            Reject(() => AssemblyDefinition.ReadNativeAssembly(image));
            var snapshot = AssemblyDefinition.ReadNativeAssembly(image, true);
            Check(snapshot.Write().SequenceEqual(image), "original wrapper image retained");
            var host = new AssemblyBuilder(new("WrapperHost" + kind, new(1, 0, 0, 0)), core);
            var factory = snapshot.MainModule.Functions.Single(f => f.Name == "Make");
            Reject(() => host.ImportReference(factory, core));
            var imported = host.ImportExperimentalCompositionReference(factory, core);
            Reject(() => host.ImportReference(snapshot.MainModule.Types.Single(t => t.Name == "Wrapper`1"), core)
                .MakeGenericInstance(imported.Signature.ReturnType.ImportedType!.TypeArguments.ToArray()));
            var main = host.AddFunction("Main"); host.EntryPoint = main;
            Reject(() => main.DeclareLocal(imported.Signature.ReturnType));
            main.Call(imported);
            main.Call(host.ImportExperimentalCompositionReference(snapshot.MainModule.Functions.Single(f => f.Name == "Echo"), core));
            main.Call(host.ImportExperimentalCompositionReference(snapshot.MainModule.Functions.Single(f => f.Name == "Consume"), core));
            main.Return();
            var hostImage = NativeModuleContainer.WriteLibraryBinary(host.WriteNativeAssembly());
            Reject(() => host.Write());
            Reject(() => RuntimeAssemblyContainer.Write(NativeModuleContainer.Read(image), core));
            var wrongKind = root.DeepClone();
            var other = kind == "Union" ? "Intersection" : "Union";
            var wrongShape = new JsonObject { [other] = Shape()[kind]!.DeepClone() };
            wrongKind["functions"]![1]!["parameters"]![0]!["Constructed"]!["arguments"]![0] = wrongShape;
            var incompatible = AssemblyDefinition.ReadNativeAssembly(Encode(wrongKind), true);
            var badHost = new AssemblyBuilder(new("WrongWrapper", new(1, 0, 0, 0)), core);
            var badMain = badHost.AddFunction("Main"); badHost.EntryPoint = badMain;
            badMain.Call(badHost.ImportExperimentalCompositionReference(incompatible.MainModule.Functions.Single(f => f.Name == "Make"), core));
            badMain.Call(badHost.ImportExperimentalCompositionReference(incompatible.MainModule.Functions.Single(f => f.Name == "Echo"), core));
            badMain.Emit(OpCode.Pop); badMain.LoadConstant(42); badMain.Return();
            Reject(() => badHost.WriteNativeAssembly());
            Reject(() => host.ImportReference(factory, core));
            Reject(() => NeoClrMetadataReference.ReadAssembly(image, bootstrap));
            if (kind == "Union") Reject(() => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(image, bootstrap));
            var reference = NeoClrMetadataReference.ReadExperimentalCompositionAssembly(image, bootstrap);
            var options = CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithRuntimeTypeOfContract(null);
            var wrongReference = NeoClrMetadataReference.ReadExperimentalCompositionAssembly(Encode(wrongKind), bootstrap);
            var wrongCompilation = Compilation.Create("WrongWrapper" + kind, [SyntaxTree.ParseText("func Main() -> int => Consume(Echo(Make()))")], [bootstrap.Reference, wrongReference], options);
            var wrongDiagnostics = wrongCompilation.GetDiagnostics();
            var wrongAssembly = (IAssemblySymbol)wrongCompilation.GetAssemblyOrModuleSymbol(wrongReference)!;
            var from = wrongAssembly.GlobalNamespace.GetMembers("Make").OfType<IMethodSymbol>().Single().ReturnType;
            var to = wrongAssembly.GlobalNamespace.GetMembers("Echo").OfType<IMethodSymbol>().Single().Parameters[0].Type;
            Check(!from.MetadataIdentityEquals(to) && !wrongCompilation.ClassifyConversion(from, to).IsImplicit &&
                wrongDiagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), "compiler wrapper invariance");
            var compilation = Compilation.Create("Wrapper" + kind, [SyntaxTree.ParseText("func Main() -> int => Consume(Echo(Make()))")], [bootstrap.Reference, reference], options);
            NeoClrEmitOptions EmitOptions(bool enabled, bool old = false) => new(new("Wrapper" + kind, new(1, 0, 0, 0)), core,
                [new(bootstrap.Reference, coreDefinition, core), new(reference, core)], allowExperimentalCompositionCalls: enabled, allowExperimentalIntersectionCalls: old);
            using var stream = new MemoryStream();
            var result = NeoClrCompilationEmitter.Emit(compilation, stream, EmitOptions(true));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            using var denied = new MemoryStream();
            Check(!NeoClrCompilationEmitter.Emit(compilation, denied, EmitOptions(false)).Success && denied.Length == 0, "nested default gate");
            if (kind == "Union")
                Check(!NeoClrCompilationEmitter.Emit(compilation, denied, EmitOptions(false, true)).Success && denied.Length == 0, "nested union rejects intersection-only option");
            Check(!NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, denied, EmitOptions(true)).Success && denied.Length == 0, "nested PE gate");
            var stored = Compilation.Create("StoredWrapper", [SyntaxTree.ParseText("func Main() -> int { let value = Make(); return Consume(value) }")], [bootstrap.Reference, reference], options);
            Check(!NeoClrCompilationEmitter.Emit(stored, denied, EmitOptions(true)).Success && denied.Length == 0, "authored wrapper storage gate");
            Directory.CreateDirectory(output);
            File.WriteAllBytes(Path.Combine(output, kind + "Provider.neox"), image);
            File.WriteAllBytes(Path.Combine(output, kind + "Host.neox"), hostImage);
            File.WriteAllBytes(Path.Combine(output, kind + "Raven.neox"), NativeModuleContainer.WriteLibraryBinary(stream.ToArray()));
        }
        Console.WriteLine("PASS wrapper composition metadata, host/Raven calls and default/CLI/storage gates");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
    private static void Reject(Action action)
    {
        try { action(); }
        catch (Exception error) when (error is InvalidDataException or ArgumentException) { return; }
        throw new Exception("expected wrapper composition rejection");
    }
}
