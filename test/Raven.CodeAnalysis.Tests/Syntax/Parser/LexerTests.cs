using System.IO;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class LexerTests
{
    [Fact]
    public void DotPrefixedDecimal_IsParsedAsNumericLiteral()
    {
        var lexer = new Lexer(new StringReader(".12"));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.NumericLiteralToken, token.Kind);
        Assert.Equal(".12", token.Text);
        var value = Assert.IsType<double>(token.Value);
        Assert.Equal(0.12d, value);
    }

    [Theory]
    [InlineData("unit", SyntaxKind.UnitKeyword)]
    [InlineData("and", SyntaxKind.AndToken)]
    [InlineData("as", SyntaxKind.AsKeyword)]
    public void Keyword_IsParsedAsKeywordToken(string text, SyntaxKind expected)
    {
        var lexer = new Lexer(new StringReader(text));
        var token = lexer.ReadToken();

        Assert.Equal(expected, token.Kind);
        Assert.Equal(text, token.Text);
    }

    [Theory]
    [InlineData("$foo")]
    [InlineData("_value")]
    [InlineData("foo_bar")]
    [InlineData("foo$bar")]
    [InlineData("$foo_bar123")]
    public void Identifier_AllowsUnderscoreAndDollar(string identifier)
    {
        var lexer = new Lexer(new StringReader(identifier));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.IdentifierToken, token.Kind);
        Assert.Equal(identifier, token.Text);
    }
}
