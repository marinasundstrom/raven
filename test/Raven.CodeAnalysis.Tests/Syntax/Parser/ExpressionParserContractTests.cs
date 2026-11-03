using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class ExpressionParserContractTests
{
    [Theory]
    [InlineData("value", SyntaxKind.IdentifierName)]
    [InlineData("42", SyntaxKind.NumericLiteralExpression)]
    [InlineData("true", SyntaxKind.TrueLiteralExpression)]
    [InlineData("null", SyntaxKind.NullLiteralExpression)]
    [InlineData("'a'", SyntaxKind.CharacterLiteralExpression)]
    [InlineData("\"text\"", SyntaxKind.StringLiteralExpression)]
    [InlineData("\"hello $name\"", SyntaxKind.InterpolatedStringExpression)]
    [InlineData("self", SyntaxKind.SelfExpression)]
    [InlineData("base", SyntaxKind.BaseExpression)]
    [InlineData("a + b * c", SyntaxKind.AddExpression)]
    [InlineData("a ?? b", SyntaxKind.NullCoalesceExpression)]
    [InlineData("a && b || c", SyntaxKind.LogicalOrExpression)]
    [InlineData("value is int", SyntaxKind.IsPatternExpression)]
    [InlineData("value as string", SyntaxKind.AsExpression)]
    [InlineData("value = 1", SyntaxKind.SimpleAssignmentExpression)]
    [InlineData("-value", SyntaxKind.UnaryMinusExpression)]
    [InlineData("!value", SyntaxKind.LogicalNotExpression)]
    [InlineData("(value)", SyntaxKind.ParenthesizedExpression)]
    [InlineData("(int)value", SyntaxKind.CastExpression)]
    [InlineData("(1, 2)", SyntaxKind.TupleExpression)]
    [InlineData("()", SyntaxKind.UnitExpression)]
    [InlineData("F(1, name: 2)", SyntaxKind.InvocationExpression)]
    [InlineData("value.Member", SyntaxKind.SimpleMemberAccessExpression)]
    [InlineData("value[0]", SyntaxKind.ElementAccessExpression)]
    [InlineData("[1, 2]", SyntaxKind.CollectionExpression)]
    [InlineData("[|1, 2|]", SyntaxKind.ArrayExpression)]
    [InlineData("default(int)", SyntaxKind.DefaultExpression)]
    [InlineData("typeof(int)", SyntaxKind.TypeOfExpression)]
    [InlineData("sizeof(int)", SyntaxKind.SizeOfExpression)]
    [InlineData("nameof(value.Member)", SyntaxKind.NameOfExpression)]
    [InlineData("if ready then 1 else 2", SyntaxKind.IfExpression)]
    [InlineData("match value { 1 => 2; _ => 3; }", SyntaxKind.MatchExpression)]
    [InlineData("value match { 1 => 2; _ => 3; }", SyntaxKind.PostfixMatchExpression)]
    [InlineData("x => x", SyntaxKind.SimpleFunctionExpression)]
    [InlineData("(x: int) => x + 1", SyntaxKind.ParenthesizedFunctionExpression)]
    [InlineData("func F(x: int) -> int { return x }", SyntaxKind.ParenthesizedFunctionExpression)]
    [InlineData("{ val x = 1; x }", SyntaxKind.Block)]
    [InlineData("await F()", SyntaxKind.AwaitExpression)]
    [InlineData("throw failure", SyntaxKind.ThrowExpression)]
    [InlineData("return 1", SyntaxKind.ReturnExpression)]
    public void ExpressionPreservesSourceAndShape(string source, SyntaxKind kind)
    {
        var parser = Parser(source);
        var result = parser.ParseExpression().CreateRed();
        Assert.Equal(kind, result.Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("F(1", SyntaxKind.CloseParenToken)]
    [InlineData("[1, 2", SyntaxKind.CloseBracketToken)]
    [InlineData("(1, 2", SyntaxKind.CloseParenToken)]
    [InlineData("[|1, 2", SyntaxKind.CloseArrayToken)]
    public void MissingClosingDelimiterPreservesInput(string source, SyntaxKind delimiter)
    {
        var parser = Parser(source);
        var result = parser.ParseExpression().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Contains(result.DescendantTokens(), token => token.IsMissing && token.Kind == delimiter);
        Assert.NotEmpty(parser.Diagnostics);
    }

    private static ExpressionSyntaxParser Parser(string source) =>
        new(new BaseParseContext(new Lexer(new StringReader(source))));
}
