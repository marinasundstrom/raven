using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class NeoClrUnitContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void UnitStorageUsesSelectedTypeWhileCallsRemainNoResult(bool selectVoid)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
            .WithTargetCoreAssemblyName("System.Runtime");
        if (selectVoid)
            options = options.WithRuntimeUnitContract(new RuntimeUnitContract("System.Runtime", "System.Void"));
        options = options.WithOptimizationLevel(OptimizationLevel.Debug).WithAllowUnsafe(false);
        Assert.Equal(selectVoid, options.RuntimeUnitContract is not null);
        var tree = SyntaxTree.ParseText("""
            public class Example {
                public static func Notify() { }
                public static func Run() {
                    Notify()
                    let value = ()
                }
            }
            """);
        var compilation = Compilation.Create("UnitContract", [tree], references.Select(MetadataReference.CreateFromFile).ToArray(), options);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var unit = tree.GetRoot().DescendantNodes().OfType<UnitExpressionSyntax>().Single();
        Assert.Equal(selectVoid ? "Void" : "Unit", compilation.GetSemanticModel(tree).GetTypeInfo(unit).Type!.Name);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        output.Position = 0;
        using var image = AssemblyDefinition.ReadAssembly(output);
        var example = image.MainModule.GetType("Example");
        Assert.Equal(MetadataType.Void, example.Methods.Single(m => m.Name == "Notify").ReturnType.MetadataType);
        Assert.Contains(example.Methods.Single(m => m.Name == "Run").Body.Variables,
            local => local.VariableType.FullName == (selectVoid ? "System.Void" : "System.Unit"));
        Assert.Equal(!selectVoid, image.MainModule.GetType("System.Unit") is not null);
        if (selectVoid)
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
