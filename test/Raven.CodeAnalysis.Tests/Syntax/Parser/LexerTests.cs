using System.IO;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;
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
    [InlineData("true", true)]
    [InlineData("false", false)]
    public void BooleanKeyword_ExposesBooleanValue(string text, bool expected)
    {
        var lexer = new Lexer(new StringReader(text));
        var token = lexer.ReadToken();

        Assert.Equal(expected ? SyntaxKind.TrueKeyword : SyntaxKind.FalseKeyword, token.Kind);
        var value = Assert.IsType<bool>(token.Value);
        Assert.Equal(expected, value);
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

    [Fact]
    public void NumericLiteral_WithUnderscores_IsParsedAsInteger()
    {
        var lexer = new Lexer(new StringReader("1_000"));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.NumericLiteralToken, token.Kind);
        Assert.Equal("1_000", token.Text);
        var value = Assert.IsType<int>(token.Value);
        Assert.Equal(1_000, value);
    }

    [Theory]
    [InlineData("\"\\u0041\"", "A")]
    [InlineData("\"\\u{1F600}\"", "\U0001F600")]
    [InlineData("\"\\U0001F642\"", "\U0001F642")]
    public void StringLiteral_SupportsUnicodeEscapes(string text, string expected)
    {
        var lexer = new Lexer(new StringReader(text));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.StringLiteralToken, token.Kind);
        var value = Assert.IsType<string>(token.Value);
        Assert.Equal(expected, value);
    }

    [Fact]
    public void StringLiteral_SupportsNonAsciiCharacters()
    {
        var sourceText = SourceText.From("ああ\"世界\"");
        using var reader = sourceText.GetTextReader(2);
        var lexer = new Lexer(reader, 2);

        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.StringLiteralToken, token.Kind);
        Assert.Equal("\"世界\"", token.Text);
        var value = Assert.IsType<string>(token.Value);
        Assert.Equal("世界", value);
    }

    [Fact]
    public void LineFeed_WithUnifiedNewLineToken_ReturnsNewLineToken()
    {
        var lexer = new Lexer(new StringReader("\n"));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.NewLineToken, token.Kind);
        Assert.Equal("\n", token.Text);
    }

    [Fact]
    public void LineFeed_WithoutUnifiedNewLineToken_ReturnsLineFeedToken()
    {
        var lexer = new Lexer(new StringReader("\n"))
        {
            UseUnifiedNewLineToken = false,
        };

        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.LineFeedToken, token.Kind);
        Assert.Equal("\n", token.Text);
    }

    [Fact]
    public void CarriageReturnLineFeed_WhenMerged_ReturnsSingleNewLineToken()
    {
        var lexer = new Lexer(new StringReader("\r\n"));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.NewLineToken, token.Kind);
        Assert.Equal("\r\n", token.Text);
    }

    [Fact]
    public void CarriageReturnLineFeed_WhenNotMerged_ReturnsSeparateTokens()
    {
        var lexer = new Lexer(new StringReader("\r\n"))
        {
            MergeCarriageReturnAndLineFeed = false,
            UseUnifiedNewLineToken = false,
        };

        var carriageReturn = lexer.ReadToken();
        Assert.Equal(SyntaxKind.CarriageReturnToken, carriageReturn.Kind);
        Assert.Equal("\r", carriageReturn.Text);

        var lineFeed = lexer.ReadToken();
        Assert.Equal(SyntaxKind.LineFeedToken, lineFeed.Kind);
        Assert.Equal("\n", lineFeed.Text);
    }
}
