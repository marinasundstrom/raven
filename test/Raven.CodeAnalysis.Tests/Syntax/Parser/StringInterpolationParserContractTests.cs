using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Green = Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class StringInterpolationParserContractTests
{
    [Theory]
    [InlineData("", false)]
    [InlineData("ordinary", false)]
    [InlineData("$", false)]
    [InlineData("$1", false)]
    [InlineData("$name", true)]
    [InlineData("${value}", true)]
    [InlineData("$`if`", true)]
    [InlineData("$``", false)]
    [InlineData("$`unfinished", false)]
    [InlineData("\\$name", false)]
    [InlineData("\\\\$name", true)]
    public void DetectsOnlyUnescapedInterpolation(string text, bool expected)
    {
        Assert.Equal(expected, StringInterpolationParser.ContainsInterpolation(text));
    }

    [Theory]
    [InlineData("\"hello $name\"", 1)]
    [InlineData("\"left ${value + 1} right\"", 1)]
    [InlineData("\"$first $second\"", 2)]
    [InlineData("\"$`if`\"", 1)]
    [InlineData("\"escaped \\$name and $other\"", 1)]
    [InlineData("\"value ${F(\"}\")}\"", 1)]
    [InlineData("\"value ${F('}')}\"", 1)]
    [InlineData("\"value ${F(/* } */ 1)}\"", 1)]
    [InlineData("\"value ${F([1, 2])}\"", 1)]
    [InlineData("\"value ${F(\"\"\"}\"\"\")}\"", 1)]
    [InlineData("\"value ${F(// }\n1)}\"", 1)]
    [InlineData("\"value ${`}`}\"", 1)]
    [InlineData("\"value ${F({ 1 })}\"", 1)]
    public void SingleLinePreservesSourceAndInterpolationBoundaries(string source, int count)
    {
        var green = StringInterpolationParser.ParseInterpolatedStringExpression(
            new Green.SyntaxToken(SyntaxKind.StringLiteralToken, source));
        var result = (InterpolatedStringExpressionSyntax)green.CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(count, result.Contents.OfType<InterpolationSyntax>().Count());
        Assert.All(result.Contents.OfType<InterpolationSyntax>(), interpolation =>
            Assert.False(interpolation.CloseBraceToken.IsMissing));
        Assert.False(StringInterpolationParser.IsUnterminatedSingleLineStringExpression(green));
    }

    [Theory]
    [InlineData("\"$name")]
    [InlineData("\"text ${value")]
    public void UnterminatedStringRetainsMissingEnd(string source)
    {
        var token = new Green.SyntaxToken(SyntaxKind.StringLiteralToken, source);
        var result = StringInterpolationParser.ParseInterpolatedStringExpression(token);
        Assert.True(StringInterpolationParser.IsUnterminatedSingleLineStringToken(token));
        Assert.True(StringInterpolationParser.IsUnterminatedSingleLineStringExpression(result));
        Assert.Equal(source, result.CreateRed().ToFullString());
    }

    [Theory]
    [InlineData("", false, "")]
    [InlineData("\"", true, "")]
    [InlineData("\"text", true, "text")]
    [InlineData("\"text\"", false, "text")]
    public void ExtractsInnerText(string raw, bool unterminated, string expected)
    {
        Assert.Equal(expected, StringInterpolationParser.GetSingleLineStringInnerText(raw, unterminated));
    }

    [Fact]
    public void PreservesOuterTrivia()
    {
        var trivia = Green.SyntaxTriviaList.Create(new[] { Green.SyntaxFactory.Whitespace("  ") });
        var token = new Green.SyntaxToken(SyntaxKind.StringLiteralToken, "\"$value\"", trivia, trivia);
        var result = (InterpolatedStringExpressionSyntax)StringInterpolationParser
            .ParseInterpolatedStringExpression(token).CreateRed();
        Assert.Equal("  ", result.StringStartToken.LeadingTrivia.Single().ToString());
        Assert.Equal("  ", result.StringEndToken.TrailingTrivia.Single().ToString());
        Assert.Empty(result.StringStartToken.TrailingTrivia);
        Assert.Empty(result.StringEndToken.LeadingTrivia);
    }

    [Fact]
    public void MultiLinePreservesRawText()
    {
        const string inner = "line\\n\n$name and ${value}";
        const string source = "\"\"\"" + inner + "\"\"\"";
        var result = (InterpolatedStringExpressionSyntax)StringInterpolationParser
            .ParseInterpolatedMultiLineStringExpression(
                new Green.SyntaxToken(SyntaxKind.MultiLineStringLiteralToken, source), inner)
            .CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(2, result.Contents.OfType<InterpolationSyntax>().Count());
        Assert.Equal("line\\n\n", ((InterpolatedStringTextSyntax)result.Contents[0]).Token.Value);
    }
}
