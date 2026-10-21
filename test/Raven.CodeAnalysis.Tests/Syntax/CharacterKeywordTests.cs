using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax;

public class CharacterKeywordTests
{
    [Theory]
    [InlineData(true, SyntaxKind.CharKeyword)]
    [InlineData(false, SyntaxKind.IdentifierToken)]
    public void CharacterAliasIsAnExplicitParseCapability(bool enabled, SyntaxKind expected)
    {
        var options = new ParseOptions { EnableCharKeyword = enabled };
        var tree = SyntaxTree.ParseText("let value: char = 'A'", options: options);
        var token = tree.GetRoot().DescendantTokens().Single(t => t.Text == "char");
        Assert.Equal(expected, token.Kind);
        Assert.Equal(enabled, tree.Options.EnableCharKeyword);
        Assert.Equal(enabled, options.WithPreprocessorSymbols(["TEST"]).EnableCharKeyword);
    }

    [Fact]
    public void DisabledAliasCanBeAnOrdinaryIdentifier()
    {
        var tree = SyntaxTree.ParseText("let char = 42", options: new ParseOptions { EnableCharKeyword = false });
        Assert.DoesNotContain(tree.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Equal(SyntaxKind.IdentifierToken, tree.GetRoot().DescendantTokens().Single(t => t.Text == "char").Kind);
    }
}
