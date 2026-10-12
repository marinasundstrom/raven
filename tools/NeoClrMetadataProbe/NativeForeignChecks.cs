using System.Text.Json;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeForeignChecks
{
    internal static void Run(string corePath, string library, string output, string? attributeSource = null)
    {
        Directory.CreateDirectory(output);
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        NeoClrMetadataReference Provider(string assemblyName)
        {
            var graph = new AssemblyBuilder(new(assemblyName, new(1, 0, 0, 0)), core);
            var marker = graph.AddClass("System.Runtime.InteropServices", "NativeImportAttribute");
            marker.AddConstructor(new MethodSignature(PrimitiveType.Void, [PrimitiveType.String])).GetILGenerator().Fail("Attribute constructor must never execute");
            marker.AddField("EntryPoint", PrimitiveType.String, FieldVisibility.Public);
            var convention = graph.AddEnum("System.Runtime.InteropServices", "NativeCallingConvention");
            convention.AddEnumMember("Cdecl", 0); convention.AddEnumMember("System", 1);
            marker.AddField("CallingConvention", convention, FieldVisibility.Public);
            var encoding = graph.AddEnum("System.Runtime.InteropServices", "NativeStringMarshalling");
            encoding.AddEnumMember("None", 0); encoding.AddEnumMember("Utf8", 1); encoding.AddEnumMember("Utf16", 2);
            marker.AddField("StringMarshalling", encoding, FieldVisibility.Public);
            var nativeType = graph.AddEnum("System.Runtime.InteropServices", "NativeType");
            nativeType.AddEnumMember("Utf8", 0); nativeType.AddEnumMember("Utf16", 1); nativeType.AddEnumMember("Bool8", 2); nativeType.AddEnumMember("Bool32", 3);
            var marshal = graph.AddClass("System.Runtime.InteropServices", "MarshalAsAttribute");
            marshal.AddConstructor(new MethodSignature(PrimitiveType.Void, [nativeType])).GetILGenerator().Fail("Attribute constructor must never execute");
            return NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(graph), bootstrap);
        }
        NeoClrMetadataReference SourceProvider(string path)
        {
            var systemDirectory = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(path)!, "../.."));
            var trees = new[] { path, Path.Combine(systemDirectory, "Attribute.rvn"), Path.Combine(systemDirectory, "AttributeUsageAttribute.rvn"), Path.Combine(systemDirectory, "AttributeTargets.rvn") }
                .Select(file => SyntaxTree.ParseText(File.ReadAllText(file))).ToArray();
            var compilation = Compilation.Create("System.Runtime", trees, [bootstrap.Reference],
                CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null));
            using var stream = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, stream,
                new(new("System.Runtime", new(1, 0, 0, 0)), core, [], bootstrapReference: bootstrap.Reference));
            if (!result.Success) throw new Exception("Attribute library: " + string.Join("\n", result.Diagnostics));
            var bytes = stream.ToArray();
            File.WriteAllBytes(Path.Combine(output, "System.Runtime.dll"), bytes);
            return NeoClrMetadataReference.ReadAssembly(bytes, bootstrap);
        }
        var serviceGraph = new AssemblyBuilder(new("RuntimeServiceSymbols", new(1, 0, 0, 0)), core);
        serviceGraph.AddFunction("RuntimeProvided", new(PrimitiveType.Void, [])).SetInternalCall();
        var serviceReference = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(serviceGraph), bootstrap);
        var serviceConsumer = Compilation.Create("RuntimeServiceConsumer", [], [bootstrap.Reference, serviceReference],
            CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name));
        _ = serviceConsumer.GetTypeByMetadataName("System.Object");
        var serviceAssembly = (IAssemblySymbol)serviceConsumer.GetAssemblyOrModuleSymbol(serviceReference)!;
        var serviceSymbol = serviceAssembly.GlobalNamespace.GetMembers("RuntimeProvided").OfType<IMethodSymbol>().Single();
        if (!serviceSymbol.IsExtern || !serviceSymbol.IsStatic || serviceSymbol.IsAbstract)
            throw new Exception("runtime internal call lost extern classification");
        Console.WriteLine("PASS runtime internal-call symbol readback without service resolution");
        var provider = attributeSource is null ? Provider("System.Runtime") : SourceProvider(attributeSource);
        var impostor = Provider("Impostor");
        var path = JsonSerializer.Serialize(library);
        string Wrap(string declaration, string call) => $"import System.Runtime.InteropServices.*\n{declaration}\nfunc Main() -> int {{ {call} }}";
        string Import(string name = "neoclr_add", string extra = "") => $"[NativeImport({path}, EntryPoint: \"{name}\"{extra})]";
        void Emit(string name, string source, bool succeeds, NeoClrMetadataReference? reference = null)
        {
            reference ??= provider;
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)], [bootstrap.Reference, reference],
                CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithOutputKind(OutputKind.ConsoleApplication).WithRuntimeTypeOfContract(null));
            using var stream = new MemoryStream();
            var result = NeoClrCompilationEmitter.Emit(compilation, stream,
                new(new(name, new(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(reference, core)], bootstrapReference: bootstrap.Reference));
            if (result.Success != succeeds || !succeeds && stream.Length != 0)
                throw new Exception(name + ": " + string.Join("\n", result.Diagnostics));
            if (!succeeds) return;
            var native = stream.ToArray();
            var artifact = RuntimeAssemblyContainer.WriteBinary(native, core);
            var reread = AssemblyDefinition.ReadNativeAssembly(artifact);
            if (reread.MainModule.Methods.Count(m => m.NativeImport is not null) != 1) throw new Exception("native import lost on reread");
            var importedReference = NeoClrMetadataReference.ReadAssembly(artifact, bootstrap);
            var inspector = Compilation.Create(name + "Inspector", [], [bootstrap.Reference, importedReference],
                CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name));
            _ = inspector.GetTypeByMetadataName("System.Object");
            var assembly = (IAssemblySymbol)inspector.GetAssemblyOrModuleSymbol(importedReference)!;
            var methods = assembly.GlobalNamespace.GetMembers().OfType<IMethodSymbol>()
                .Concat(assembly.GlobalNamespace.GetTypeMembers().SelectMany(t => t.GetMembers().OfType<IMethodSymbol>()))
                .Where(m => m.MethodKind == MethodKind.Ordinary).ToArray();
            if (methods.Count(m => m.IsExtern) != 1 || methods.Single(m => m.Name == "Main").IsExtern)
                throw new Exception(name + ": imported native/managed method classification lost");
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            File.WriteAllBytes(Path.Combine(output, name + ".dll"), artifact);
            Console.WriteLine("PASS " + name);
        }
        Emit("ForeignAdd", Wrap(Import() + "\nextern func Add(a: int, b: int) -> int;", "return Add(19, 23)"), true);
        Emit("ForeignStrings", Wrap(Import("neoclr_utf8_pair", ", StringMarshalling: NativeStringMarshalling.Utf8") +
            "\nextern func Pair(a: string, bias: int, b: string) -> int;", "return Pair(\"hé🦀\", 41, \"世界\")"), true);
        Emit("ForeignWide", Wrap(Import("neoclr_utf16_check", ", StringMarshalling: NativeStringMarshalling.Utf16") +
            "\nextern func Check(text: string) -> int;", "return Check(\"hé🦀\")"), true);
        Emit("ForeignDefaultName", Wrap($"[NativeImport({path})]\nextern func neoclr_add(a: int, b: int) -> int;", "return neoclr_add(20, 22)"), true);
        Emit("ForeignMissing", Wrap(Import("missing_export") + "\nextern func Missing() -> int;", "return Missing()"), true);
        Emit("ForeignSystem", Wrap(Import("neoclr_system_add", ", CallingConvention: NativeCallingConvention.System") + "\nextern func Add(a: int, b: int) -> int;", "return Add(19, 23)"), true);
        Emit("ForeignMixedEncoding", Wrap(Import("neoclr_mixed_encoding", ", StringMarshalling: NativeStringMarshalling.Utf16") +
            "\nextern func Mixed([MarshalAs(NativeType.Utf8)] a: string, b: string) -> int;", "return Mixed(\"hé🦀\", \"hé🦀\")"), true);
        foreach (var (width, symbol) in new[] { ("Bool8", "neoclr_bool8"), ("Bool32", "neoclr_bool32") })
        {
            Emit("Foreign" + width, Wrap(Import(symbol) + $"\n[return: MarshalAs(NativeType.{width})]\nextern func Test([MarshalAs(NativeType.{width})] value: bool) -> bool;",
                "if Test(true) && !Test(false) { return 42 }\nreturn 0"), true);
        }
        Emit("ForeignStatic", Wrap("static class Native { " + Import() + "\npublic static extern func Add(a: int, b: int) -> int; }", "return Native.Add(19, 23)"), true);
        Emit("RejectWrongMarshalType", Wrap(Import() + "\nextern func F([MarshalAs(NativeType.Bool8)] value: int) -> int;", "return 0"), false);
        Emit("RejectResultString", Wrap(Import() + "\n[return: MarshalAs(NativeType.Utf8)]\nextern func F() -> string;", "return 0"), false);
        Emit("RejectStringDefault", Wrap(Import() + "\nextern func F(text: string) -> int;", "return F(\"x\")"), false);
        Emit("RejectBoolean", Wrap(Import() + "\nextern func F() -> bool;", "return 0"), false);
        Emit("RejectStringReturn", Wrap(Import(extra: ", StringMarshalling: NativeStringMarshalling.Utf8") + "\nextern func F() -> string;", "return 0"), false);
        Emit("RejectBody", Wrap(Import() + "\nextern func F() -> int { return 0 }", "return F()"), false);
        Emit("RejectImpostor", Wrap(Import() + "\nextern func F() -> int;", "return F()"), false, impostor);
        Emit("RejectManagedBody", Wrap(Import() + "\nfunc F() -> int { return 0 }", "return F()"), false);
        Emit("RejectNoAttribute", Wrap("extern func F() -> int;", "return F()"), false);
        Console.WriteLine("PASS native foreign attribute positive/negative checks");
    }
}
