using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public sealed class RefStructDeclarationParserTests
{
    [Fact]
    public void RefStructDeclaration_ParsesRefModifier()
    {
        var tree = SyntaxTree.ParseText("ref struct Buffer<T> {}");

        var declaration = Assert.IsType<StructDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));
        Assert.Contains(declaration.Modifiers, modifier => modifier.IsKind(SyntaxKind.RefKeyword));
        Assert.Equal("Buffer", declaration.Identifier.ValueText);
        Assert.Single(declaration.TypeParameterList!.Parameters);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RefStructDeclaration_ParsesInNamespace()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Buffers {
                public ref struct Buffer {}
            }
            """);

        var declaration = tree.GetRoot().DescendantNodes().OfType<StructDeclarationSyntax>().Single();
        Assert.Collection(
            declaration.Modifiers,
            modifier => Assert.True(modifier.IsKind(SyntaxKind.PublicKeyword)),
            modifier => Assert.True(modifier.IsKind(SyntaxKind.RefKeyword)));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RefStructDeclaration_ParsesAsNestedType()
    {
        var tree = SyntaxTree.ParseText("""
            class Container {
                ref struct Buffer {}
            }
            """);

        var declaration = tree.GetRoot().DescendantNodes().OfType<StructDeclarationSyntax>().Single();
        Assert.Contains(declaration.Modifiers, modifier => modifier.IsKind(SyntaxKind.RefKeyword));
        Assert.Empty(tree.GetDiagnostics());
    }
}
