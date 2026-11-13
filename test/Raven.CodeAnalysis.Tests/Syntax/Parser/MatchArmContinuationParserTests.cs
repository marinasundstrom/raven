using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class MatchArmContinuationParserTests
{
    [Theory]
    [InlineData("value\n    .Trim()\n    .ToString()", "value.Trim().ToString()")]
    [InlineData("items\n    .Select(func(x) => x)", "items.Select(func(x) => x)")]
    public void FluentChainContinuesArmAcrossNewlines(string body, string expected)
    {
        var match = Parse($"value match {{\n .First => {body}\n .Second => other\n}}");
        Assert.Equal(2, match.Arms.Count);
        Assert.IsType<InvocationExpressionSyntax>(match.Arms[0].Expression);
        Assert.Equal(expected.Replace(" ", ""), string.Concat(match.Arms[0].Expression.DescendantTokens().Select(token => token.Text)));
    }

    [Theory]
    [InlineData(".Second")]
    [InlineData(".Second(value)")]
    [InlineData(".Second(value) when value > 0")]
    [InlineData(".Second when ready")]
    public void DotPatternWithArrowStartsNextArm(string pattern)
    {
        var match = Parse($"value match {{\n .First => result\n {pattern} => other\n}}");
        Assert.Equal(2, match.Arms.Count);
        Assert.IsType<IdentifierNameSyntax>(match.Arms[0].Expression);
    }

    private static PostfixMatchExpressionSyntax Parse(string text)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(text)));
        var parser = new ExpressionSyntaxParser(context);
        var result = Assert.IsType<PostfixMatchExpressionSyntax>(parser.ParseExpression().CreateRed());
        Assert.Empty(parser.Diagnostics);
        return result;
    }
}
