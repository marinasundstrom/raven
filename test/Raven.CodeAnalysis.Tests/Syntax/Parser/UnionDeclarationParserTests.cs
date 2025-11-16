using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class UnionDeclarationParserTests
{
    [Fact]
    public void UnionDeclaration_WithCases_ParsesCases()
    {
        const string source = "union Token { Identifier(text: string) Unknown }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));
        Assert.Equal("Token", declaration.Identifier.Text);

        var cases = declaration.Cases;
        Assert.Collection(
            cases,
            first =>
            {
                Assert.Equal("Identifier", first.Identifier.Text);
                var parameter = Assert.Single(first.ParameterList!.Parameters);
                Assert.Equal("text", parameter.Identifier.Text);
            },
            second => Assert.Equal("Unknown", second.Identifier.Text));

        Assert.Empty(tree.GetDiagnostics());
    }
}
