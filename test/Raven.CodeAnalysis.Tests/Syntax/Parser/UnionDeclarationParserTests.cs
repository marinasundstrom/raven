using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class UnionDeclarationParserTests
{
    [Fact]
    public void UnionDeclaration_WithCases_ParsesCaseList()
    {
        var source = "union Token { Identifier(text: string) Unknown }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal("Token", declaration.Identifier.Text);
        Assert.Collection(
            declaration.Cases,
            first =>
            {
                Assert.Equal("Identifier", first.Identifier.Text);
                Assert.NotNull(first.ParameterList);
            },
            second =>
            {
                Assert.Equal("Unknown", second.Identifier.Text);
                Assert.Null(second.ParameterList);
            });
    }

    [Fact]
    public void UnionDeclaration_WithTypeParameters_ParsesGenerics()
    {
        var source = "union Result<T> { Ok(value: T) }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        var okCase = Assert.Single(declaration.Cases);
        Assert.Equal("Ok", okCase.Identifier.Text);
        Assert.NotNull(okCase.ParameterList);
    }
}
