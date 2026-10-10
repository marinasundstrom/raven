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
        Console.WriteLine("PASS native attribute import, all supported member kinds, typed/named values and cross-assembly usage diagnostics");
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
