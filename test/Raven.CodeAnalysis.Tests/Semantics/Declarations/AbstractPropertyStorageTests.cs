using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class AbstractPropertyStorageTests
{
    [Theory]
    [InlineData("abstract val Value: int { get; }", false)]
    [InlineData("abstract val Value: int { get; }", true)]
    [InlineData("abstract var Value: int { get; set; }", false)]
    [InlineData("abstract var Value: int { get; set; }", true)]
    [InlineData("abstract val Value: int", false)]
    [InlineData("abstract val Value: int", true)]
    [InlineData("abstract var Value: int", false)]
    [InlineData("abstract var Value: int", true)]
    public void AbstractAccessors_DoNotCreateAutoPropertyStorage(string declaration, bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText("abstract class Contract { " + declaration + " }");
        var compilation = Compilation.Create("AbstractStorage", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var property = Assert.IsType<SourcePropertySymbol>(model.GetDeclaredSymbol(syntax));
        Assert.Null(property.BackingField);
        Assert.False(property.IsAutoProperty);
        Assert.True(property.GetMethod!.IsAbstract);
        Assert.Same(property, model.GetDeclaredSymbol(syntax));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Null(property.BackingField);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrAbstractPropertyStorageTests
{
    [Fact]
    public void NeoClrWithHostReferences_AbstractPropertyIsAContractWithoutStorage()
    {
        var tree = SyntaxTree.ParseText("abstract class Contract { abstract var Value: int { get; set; } }");
        var compilation = Compilation.Create("NeoClrAbstractStorage", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var property = Assert.IsType<SourcePropertySymbol>(model.GetDeclaredSymbol(syntax));
        Assert.Null(property.BackingField);
        Assert.False(property.IsAutoProperty);
        Assert.True(property.GetMethod!.IsAbstract);
        Assert.True(property.SetMethod!.IsAbstract);
    }
}
