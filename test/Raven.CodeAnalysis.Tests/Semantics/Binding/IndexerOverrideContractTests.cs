using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class IndexerOverrideContractTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void DotNetGetterRetainsOverrideIdentity(bool baseArrow, bool derivedArrow)
        => AssertGetter(TargetPlatform.DotNet, baseArrow, derivedArrow);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClrGetterRetainsOverrideIdentity(bool derivedArrow)
        => AssertGetter(TargetPlatform.NeoCLR, true, derivedArrow);

    private static void AssertGetter(TargetPlatform target, bool baseArrow, bool derivedArrow)
    {
        var compilation = Create(target, baseArrow, derivedArrow);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<IndexerDeclarationSyntax>().Last();
        var property = Assert.IsAssignableFrom<PropertySymbol>(model.GetDeclaredSymbol(syntax));
        var getter = Assert.IsType<SourceMethodSymbol>(property.GetMethod);
        Assert.True(getter.IsOverride);
        Assert.True(getter.IsVirtual);
        Assert.Equal("Base", getter.OverriddenMethod?.ContainingType.Name);
        Assert.Same(getter, Assert.Single(property.ContainingType!.GetMembers("get_Item").OfType<MethodSymbol>()));
    }

    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void GetterDispatchesThroughBaseReference(bool baseArrow, bool derivedArrow)
    {
        var compilation = Create(TargetPlatform.DotNet, baseArrow, derivedArrow);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(12, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("open class Base { val self[index: int]: int => index }", "RAV0307")]
    [InlineData("open class Root { virtual val self[index: int]: int => index }; open class Base : Root { final override val self[index: int]: int => index }", "RAV0310")]
    public void ExpressionBodiedOverrideValidatesBaseGetter(string baseDeclaration, string expectedDiagnostic)
    {
        var tree = SyntaxTree.ParseText(baseDeclaration + "\nclass Derived : Base { override val self[index: int]: int => index + 1 }");
        var compilation = Compilation.Create("InvalidIndexerOverride", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == expectedDiagnostic);
    }

    private static Compilation Create(TargetPlatform target, bool baseArrow, bool derivedArrow)
    {
        var baseBody = baseArrow ? "=> index" : "{ get { return index } }";
        var derivedBody = derivedArrow ? "=> index + 10" : "{ get { return index + 10 } }";
        var source = $$"""
            open class Base { virtual val self[index: int]: int {{baseBody}} }
            class Derived : Base { override val self[index: int]: int {{derivedBody}} }
            class Probe {
                static func Run() -> int {
                    let value: Base = Derived()
                    return value[2]
                }
            }
            """;
        return Compilation.Create("IndexerOverride" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
    }
}
