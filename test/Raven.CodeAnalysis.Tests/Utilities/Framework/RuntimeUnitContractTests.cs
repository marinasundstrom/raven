using Raven.CodeAnalysis.Testing;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RuntimeUnitContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void UnitStorageUsesSelectedTypeWhileCallsRemainNoResult(bool selectValueTuple)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
            .WithTargetCoreAssemblyName("System.Runtime");
        if (selectValueTuple)
            options = options.WithRuntimeUnitContract(new RuntimeUnitContract("System.Runtime", "System.ValueTuple"));
        options = options.WithOptimizationLevel(OptimizationLevel.Debug).WithAllowUnsafe(false);
        Assert.Equal(selectValueTuple, options.RuntimeUnitContract is not null);
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public class Example {
                public static func Notify() { }
                public static func Run() -> int {
                    Notify()
                    let value = ()
                    let values = List<()>()
                    values.Add(value)
                    values.Add(Notify())
                    return values.Count
                }
            }
            """);
        var compilation = Compilation.Create("UnitContract", [tree], references.Select(MetadataReference.CreateFromFile).ToArray(), options);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var unit = tree.GetRoot().DescendantNodes().OfType<UnitExpressionSyntax>().Single();
        Assert.Equal(selectValueTuple ? "ValueTuple" : "Unit", compilation.GetSemanticModel(tree).GetTypeInfo(unit).Type!.Name);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using (var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References))
            Assert.Equal(2, loaded.Assembly.GetType("Example")!.GetMethod("Run")!.Invoke(null, null));
        output.Position = 0;
        using var image = AssemblyDefinition.ReadAssembly(output);
        var example = image.MainModule.GetType("Example");
        Assert.Equal(MetadataType.Void, example.Methods.Single(m => m.Name == "Notify").ReturnType.MetadataType);
        Assert.Contains(example.Methods.Single(m => m.Name == "Run").Body.Variables,
            local => local.VariableType.FullName == (selectValueTuple ? "System.ValueTuple" : "System.Unit"));
        Assert.Equal(!selectValueTuple, image.MainModule.GetType("System.Unit") is not null);
        if (selectValueTuple)
            Assert.DoesNotContain(image.MainModule.GetTypeReferences(), type => type.FullName == "System.Unit");
    }

    [Theory]
    [InlineData("System.Int32")]
    [InlineData("System.String")]
    [InlineData("Missing")]
    public void InvalidUnitContractIsADiagnostic(string type)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
            .WithTargetCoreAssemblyName("System.Runtime")
            .WithRuntimeUnitContract(new RuntimeUnitContract("System.Runtime", type));
        var compilation = Compilation.Create("BadUnitContract", [SyntaxTree.ParseText("class Example { }")],
            references.Select(MetadataReference.CreateFromFile).ToArray(), options);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAVT003");
    }
}
