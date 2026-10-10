using System.Security.Cryptography;
using System.Text;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeAttributeChecks
{
    internal static void Run(string corePath)
    {
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        var graph = new AssemblyBuilder(new("NativeAttributes", new(1, 0, 0, 0)), core);
        var targets = graph.CreateEnumReference(core, core, Convert.ToHexString(SHA256.HashData(bytes)), "System", "AttributeTargets");
        var usage = graph.Definition.MainModule.ImportReference(core, "System", "AttributeUsageAttribute");
        CustomAttributeDefinition Policy(int validOn, bool? multiple = null) => new(usage,
            [new(targets, validOn)], multiple is null ? [] :
            [new("AllowMultiple", false, new(PrimitiveType.Boolean, multiple.Value)), new("Inherited", false, new(PrimitiveType.Boolean, false))]);
        TypeBuilder Marker(string name, CustomAttributeDefinition? policy = null, TypeBuilder? parent = null)
        {
            var type = parent is null ? graph.AddClass("Tests", name) : graph.AddClass("Tests", name, parent);
            type.AddConstructor(new MethodSignature(PrimitiveType.Void, [])).GetILGenerator().Fail("Attribute constructors must not execute");
            if (policy is not null) type.AddCustomAttribute(policy);
            return type;
        }
        var single = Marker("MethodOnlyAttribute", Policy(64));
        var repeated = Marker("RepeatedAttribute", Policy(64, true));
        Marker("InheritedPolicyAttribute", parent: repeated);
        Marker("ReplacedPolicyAttribute", Policy(4), repeated);
        Marker("DefaultAttribute");
        var lookalike = graph.AddClass("Tests", "AttributeUsageAttribute");
        var lookalikeConstructor = lookalike.AddConstructor(new MethodSignature(PrimitiveType.Void, [PrimitiveType.Int32]));
        lookalikeConstructor.GetILGenerator().Fail("Lookalike metadata must not execute");
        Marker("LookalikePolicyAttribute", new CustomAttributeDefinition(lookalikeConstructor.Definition, [new(PrimitiveType.Int32, 64)]));
        var tagged = new CustomAttributeDefinition(single.Methods.Single().Definition, []);
        var fixture = graph.AddClass("Tests", "Fixture");
        fixture.AddCustomAttribute(tagged);
        fixture.AddField("Number", PrimitiveType.Int32, FieldVisibility.Public).Definition.CustomAttributes.Add(tagged);
        var getter = fixture.AddMethod("get_Number", new(PrimitiveType.Int32, []));
        getter.LoadConstant(42); getter.Return();
        fixture.AddProperty("Number", PrimitiveType.Int32, getter, null).Definition.CustomAttributes.Add(tagged);
        var member = fixture.AddMethod("Read", new(PrimitiveType.Int32, [PrimitiveType.Int32]));
        member.LoadArgument(0); member.Return(); member.AddCustomAttribute(tagged);
        member.Definition.GetParameterCustomAttributes(0).Add(tagged);
        var free = graph.DefineModule("Tests").AddFunction("Read", new(PrimitiveType.Int32, []));
        free.LoadConstant(42); free.Return(); free.AddCustomAttribute(tagged);
        var constructor = fixture.AddConstructor(new MethodSignature(PrimitiveType.Void, []));
        constructor.GetILGenerator().Fail("Discovery must not execute constructors"); constructor.AddCustomAttribute(tagged);
        var color = graph.AddEnum("Tests", "Color");
        color.AddEnumMember("Red", 7);
        var note = graph.AddClass("Tests", "NoteAttribute");
        note.AddCustomAttribute(Policy(32767, true));
        var noteConstructor = note.AddConstructor(new MethodSignature(PrimitiveType.Void, [PrimitiveType.String, PrimitiveType.Int32, PrimitiveType.Boolean, color]));
        noteConstructor.SetNullableAnnotation(0, new([2]));
        noteConstructor.GetILGenerator().Fail("Source metadata must not execute attribute constructors");
        note.AddField("Active", PrimitiveType.Boolean, FieldVisibility.Public);
        var labelGet = note.AddInstanceMethod("get_Label", new(PrimitiveType.String, []));
        labelGet.GetILGenerator().Fail("Attribute metadata must not execute getters");
        var labelSet = note.AddInstanceMethod("set_Label", new(PrimitiveType.Void, [PrimitiveType.String]));
        labelSet.GetILGenerator().Fail("Attribute metadata must not execute setters");
        note.AddProperty("Label", PrimitiveType.String, labelGet, labelSet);
        var wide = graph.AddClass("Tests", "WideAttribute");
        wide.AddConstructor(new MethodSignature(PrimitiveType.Void, [PrimitiveType.Int64])).GetILGenerator().Fail("unsupported argument");
        var native = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(graph), bootstrap);
        Compilation Create(string source) => Compilation.Create("Consumer", [SyntaxTree.ParseText(source)],
            [bootstrap.Reference, native], CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name)
                .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null));
        var compilation = Create("");
        var marker = compilation.GetTypeByMetadataName("Tests.MethodOnlyAttribute")!;
        var data = marker.GetAttributes().Single();
        Check(data.AttributeClass.ToFullyQualifiedMetadataName() == "System.AttributeUsageAttribute", "usage identity lost");
        Check(data.ConstructorArguments.Single().Kind == TypedConstantKind.Enum &&
            data.ConstructorArguments.Single().Type?.ToFullyQualifiedMetadataName() == "System.AttributeTargets" &&
            data.ConstructorArguments.Single().Value is 64, "enum constant changed");
        var repeatedData = compilation.GetTypeByMetadataName("Tests.RepeatedAttribute")!.GetAttributes().Single();
        Check(repeatedData.NamedArguments.Single(a => a.Key == "AllowMultiple").Value.Value is true &&
            repeatedData.NamedArguments.Single(a => a.Key == "Inherited").Value.Value is false, "named options lost");
        var imported = compilation.GetTypeByMetadataName("Tests.Fixture")!;
        foreach (var symbol in new ISymbol[] { imported, imported.GetMembers("Number").OfType<IFieldSymbol>().Single(),
            imported.GetMembers("Number").OfType<IPropertySymbol>().Single(), imported.GetMembers("Read").Single(),
            imported.Constructors.Single(), imported.GetMembers("Read").OfType<IMethodSymbol>().Single().Parameters.Single() })
            Check(SymbolEqualityComparer.Default.Equals(symbol.GetAttributes().Single().AttributeClass, marker), "member attribute identity lost");
        var nativeAssembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(native)!;
        var module = nativeAssembly.GlobalNamespace.GetMembers().OfType<INamespaceSymbol>().Single(n => n.Name == "Tests");
        Check(module.GetMembers("Read").Single().GetAttributes().Single().AttributeClass.Name == "MethodOnlyAttribute", "free function attribute lost");
        foreach (var (source, expected) in new (string, string?)[]
        {
            ("[Tests.MethodOnly] func Test() -> int { return 42 }", null),
            ("[Tests.MethodOnly] class Bad { }", "RAV0502"),
            ("[Tests.MethodOnly][Tests.MethodOnly] func Test() -> int { return 42 }", "RAV0503"),
            ("[Tests.Repeated][Tests.Repeated] func Test() -> int { return 42 }", null),
            ("[Tests.InheritedPolicy][Tests.InheritedPolicy] func Test() -> int { return 42 }", null),
            ("[Tests.ReplacedPolicy] func Test() -> int { return 42 }", "RAV0502"),
            ("[Tests.ReplacedPolicy][Tests.ReplacedPolicy] class Bad { }", "RAV0503"),
            ("[Tests.Default][Tests.Default] class Bad { }", "RAV0503"),
            ("[Tests.LookalikePolicy] class Good { }", null),
        })
        {
            var errors = Create(source).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            Check(expected is null ? errors.Length == 0 : errors.Length == 1 && errors[0].Id == expected,
                source + ": expected " + (expected ?? "no errors") + "; got " + string.Join("; ", errors.Select(d => d.ToString())));
        }
        foreach (var corruption in new[] { "constructor", "member", "type" })
        {
            var json = JsonNode.Parse(graph.WriteNativeAssembly())!;
            var attribute = json["types"]!.AsArray().SelectMany(t => t!["custom_attributes"]?.AsArray() ?? [])
                .First(a => a!["named_arguments"] is not null)!;
            if (corruption == "constructor")
            {
                attribute["constructor"]!["parameters"]![0] = "Boolean";
                attribute["arguments"]![0] = new JsonObject { ["Boolean"] = true };
            }
            else if (corruption == "member") attribute["named_arguments"]![0]!["name"] = "Missing";
            else attribute["named_arguments"]![0]!["value"] = new JsonObject { ["String"] = "wrong" };
            var malformed = NeoClrMetadataReference.ReadAssembly(NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(json.ToJsonString())), bootstrap);
            var invalid = Compilation.Create("InvalidMetadata", [], [bootstrap.Reference, malformed], compilation.Options);
            Check(invalid.GetDiagnostics().Any(d => d.Id == "RAVT003"), "malformed attribute metadata did not produce a target diagnostic: " + corruption);
        }
        var annotated = Create("""
            import Tests.*
            [Default]
            [Note("text ☃", 42, true, Color.Red, Label: "named", Active: true)]
            [Note(null, -7, false, Color.Red)]
            public class Subject {
                [Default]
                public field Number: int;
                [Default]
                var Name: int { [Default] get; [Default] set; }
                [Default]
                init([Default] number: int) { Number = number }
                [MethodOnly]
                func Read([Default] value: int) -> int { return value }
            }
            [Default]
            public interface Contract {
                [MethodOnly]
                func Read([Default] value: int) -> int;
                [Default]
                val Number: int { [Default] get; }
            }
            [Default]
            public enum Status { Ready = 1 }
            [Repeated][Repeated]
            func Read([Default] value: int) -> int { return value }
            """);
        using var emitted = new MemoryStream();
        var emittedResult = NeoClrCompilationEmitter.EmitMetadataAssembly(annotated, emitted,
            new(new("Consumer", new(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(native, core)], bootstrapReference: bootstrap.Reference));
        Check(emittedResult.Success, string.Join("\n", emittedResult.Diagnostics.Select(d => d + " at " + d.Location.GetLineSpan().StartLinePosition.Line)));
        var roundtrip = AssemblyDefinition.ReadNativeAssembly(emitted.ToArray());
        var emittedType = roundtrip.MainModule.Types.Single(t => t.Name == "Subject");
        Check(emittedType.CustomAttributes.Count == 3 && emittedType.CustomAttributes[0].AttributeType.Name == "DefaultAttribute", "source type annotation missing");
        Check(emittedType.Fields.Single(f => f.Name == "Number").CustomAttributes.Count == 1, "source field annotation missing");
        Check(emittedType.Fields.Where(f => f.Name != "Number").All(f => f.CustomAttributes.Count == 0), "property annotations leaked to backing fields");
        Check(emittedType.Properties.Single().CustomAttributes.Count == 1, "source property annotation missing");
        Check(emittedType.Methods.All(m => m.CustomAttributes.Count == 1), "source method/accessor/constructor annotation missing");
        Check(emittedType.Methods.Single(m => m.Name == "Read").GetParameterCustomAttributes(0).Count == 1, "source parameter annotation missing");
        Check(emittedType.Methods.Single(m => m.Name == ".ctor").GetParameterCustomAttributes(0).Count == 1, "source constructor parameter annotation missing");
        Check(roundtrip.MainModule.Functions.Single().CustomAttributes.Count == 2, "source repeated function annotations lost");
        Check(roundtrip.MainModule.Functions.Single().GetParameterCustomAttributes(0).Count == 1, "source function parameter annotation missing");
        var fixedData = emittedType.CustomAttributes[1];
        Check(fixedData.GetArguments()[0].Value is "text ☃" && fixedData.GetArguments()[1].Value is 42 &&
            fixedData.GetArguments()[2].Value is true && fixedData.GetArguments()[3].Value is 7 &&
            fixedData.GetArguments()[3].Type.ReferencedType?.Name == "Color", "source fixed/enum arguments changed");
        Check(fixedData.GetNamedArguments().Count == 2 && fixedData.GetNamedArguments()[0].MemberName == "Label" &&
            !fixedData.GetNamedArguments()[0].IsField && fixedData.GetNamedArguments()[1].IsField, "source named argument kinds/order changed");
        Check(emittedType.CustomAttributes[2].GetArguments()[0].Value is null, "source null string argument changed");
        var contract = roundtrip.MainModule.Types.Single(t => t.Name == "Contract");
        Check(contract.CustomAttributes.Count == 1 && contract.Properties.Single().CustomAttributes.Count == 1 &&
            contract.Methods.All(m => m.CustomAttributes.Count == 1), "source interface annotations missing: " + contract.CustomAttributes.Count + "/" + contract.Properties.Single().CustomAttributes.Count + "/" + string.Join(",", contract.Methods.Select(m => m.Name + ":" + m.CustomAttributes.Count)));
        Check(roundtrip.MainModule.Types.Single(t => t.Name == "Status").CustomAttributes.Count == 1, "source enum annotation missing");
        foreach (var source in new[] {
            "[Tests.Wide(42L)] public class Bad { }",
            "[return: Tests.Default] func Bad() -> int { return 0 }" })
        {
            using var rejected = new MemoryStream();
            rejected.WriteByte(77);
            var failure = NeoClrCompilationEmitter.EmitMetadataAssembly(Create(source), rejected,
                new(new("Consumer", new(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(native, core)], bootstrapReference: bootstrap.Reference));
            Check(!failure.Success && failure.Diagnostics.Any(d => d.Id == "NEOMETA001") &&
                rejected.ToArray().SequenceEqual(new byte[] { 77 }), "unsupported attribute must diagnose without output: " + string.Join("; ", failure.Diagnostics));
        }
        var localSource = Create("""
            module System
            public abstract class Attribute {
                protected init() { }
            }
            public class LocalAttribute : Attribute {
                init(text: string) { }
            }
            [Local("owned")]
            public class Annotated { }
            """);
        using var localImage = new MemoryStream();
        var localResult = NeoClrCompilationEmitter.EmitMetadataAssembly(localSource, localImage,
            new(new("Consumer", new(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(native, core)], bootstrapReference: bootstrap.Reference));
        Check(localResult.Success, "co-owned source attribute: " + string.Join("; ", localResult.Diagnostics));
        var localSnapshot = AssemblyDefinition.ReadNativeAssembly(localImage.ToArray());
        Check(localSnapshot.MainModule.Types.Single(t => t.Name == "Annotated").CustomAttributes.Single().GetArguments().Single().Value is "owned",
            "co-owned attribute constructor identity/argument lost");
        Console.WriteLine("PASS native source type/member/parameter/function annotation emission");
        Console.WriteLine("PASS native attribute import, all supported member kinds, typed/named values and cross-assembly usage diagnostics");
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
