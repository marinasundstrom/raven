using System;
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
    [InlineData("-42", typeof(int), -42)]
    [InlineData("-2147483649", typeof(long), -2147483649L)]
    [InlineData("-1.5", typeof(double), -1.5d)]
    [InlineData("-.5", typeof(double), -0.5d)]
    [InlineData("-1.5f", typeof(float), -1.5f)]
    [InlineData("-1.5e2", typeof(double), -150d)]
    public void NegativeNumericLiteral_IsParsedWithCorrectValue(string text, Type expectedType, object expectedValue)
    {
        var lexer = new Lexer(new StringReader(text));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.NumericLiteralToken, token.Kind);
        Assert.Equal(text, token.Text);
        Assert.Equal(expectedType, token.Value?.GetType());
        Assert.Equal(expectedValue, token.Value);
    }

    [Theory]
    [InlineData("unit", SyntaxKind.UnitKeyword)]
    [InlineData("bool", SyntaxKind.BoolKeyword)]
    [InlineData("char", SyntaxKind.CharKeyword)]
    [InlineData("sbyte", SyntaxKind.SByteKeyword)]
    [InlineData("byte", SyntaxKind.ByteKeyword)]
    [InlineData("short", SyntaxKind.ShortKeyword)]
    [InlineData("ushort", SyntaxKind.UShortKeyword)]
    [InlineData("int", SyntaxKind.IntKeyword)]
    [InlineData("uint", SyntaxKind.UIntKeyword)]
    [InlineData("long", SyntaxKind.LongKeyword)]
    [InlineData("ulong", SyntaxKind.ULongKeyword)]
    [InlineData("nint", SyntaxKind.NIntKeyword)]
    [InlineData("nuint", SyntaxKind.NUIntKeyword)]
    [InlineData("float", SyntaxKind.FloatKeyword)]
    [InlineData("double", SyntaxKind.DoubleKeyword)]
    [InlineData("decimal", SyntaxKind.DecimalKeyword)]
    [InlineData("string", SyntaxKind.StringKeyword)]
    [InlineData("object", SyntaxKind.ObjectKeyword)]
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
    public void EscapedReservedWord_IsParsedAsIdentifierToken()
    {
        var lexer = new Lexer(new StringReader("@int"));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.IdentifierToken, token.Kind);
        Assert.Equal("@int", token.Text);
        var value = Assert.IsType<string>(token.Value);
        Assert.Equal("int", value);
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

    [Theory]
    [InlineData("\"Saw \\\"text\\\"\"", "Saw \"text\"")]
    [InlineData("\"It\\'s fine\"", "It's fine")]
    [InlineData("\"Tabbed\\tvalue\"", "Tabbed\tvalue")]
    public void StringLiteral_DecodesCommonEscapes(string text, string expected)
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

    [Theory]
    [InlineData("序章\"こんにちは ${name} 世界\"", "\"こんにちは ${name} 世界\"", "こんにちは ${name} 世界")]
    [InlineData("مقدمة\"مرحبا ${name} بالعالم\"", "\"مرحبا ${name} بالعالم\"", "مرحبا ${name} بالعالم")]
    public void StringLiteral_WithInterpolation_SupportsNonAsciiCharacters(string source, string expectedText, string expectedValue)
    {
        var start = source.IndexOf('\"');
        var sourceText = SourceText.From(source);
        using var reader = sourceText.GetTextReader(start);
        var lexer = new Lexer(reader, start);

        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.StringLiteralToken, token.Kind);
        Assert.Equal(expectedText, token.Text);
        var value = Assert.IsType<string>(token.Value);
        Assert.Equal(expectedValue, value);
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

    [Fact]
    public void ShiftLeftOperator_IsLexedAsSingleToken()
    {
        var lexer = new Lexer(new StringReader("a<<1"));

        Assert.Equal(SyntaxKind.IdentifierToken, lexer.ReadToken().Kind);
        Assert.Equal(SyntaxKind.LessThanLessThanToken, lexer.ReadToken().Kind);
        Assert.Equal(SyntaxKind.NumericLiteralToken, lexer.ReadToken().Kind);
    }

    [Fact]
    public void ShiftRightOperator_IsLexedAsSingleToken()
    {
        var lexer = new Lexer(new StringReader("a>>1"));

        Assert.Equal(SyntaxKind.IdentifierToken, lexer.ReadToken().Kind);
        Assert.Equal(SyntaxKind.GreaterThanGreaterThanToken, lexer.ReadToken().Kind);
        Assert.Equal(SyntaxKind.NumericLiteralToken, lexer.ReadToken().Kind);
    }
}
