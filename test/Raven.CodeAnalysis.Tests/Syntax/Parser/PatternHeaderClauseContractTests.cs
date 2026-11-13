using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class PatternHeaderClauseContractTests
{
    [Theory]
    [InlineData("int")]
    [InlineData("int or ulong")]
    [InlineData("(int) -> bool")]
    public void TypePatternLeavesNextLineMemberModifierUnconsumed(string text)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(text + "\n\nprivate static func Next() -> bool => true")));
        var parser = new PatternSyntaxParser(context);
        var pattern = parser.ParsePattern().CreateRed();
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(text, pattern.ToString());
        Assert.Equal(SyntaxKind.PrivateKeyword, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("Widget { result }")]
    [InlineData("Widget { owner.Value }")]
    [InlineData("Widget { }")]
    public void HeaderPatternLeavesBodyBraceUnconsumed(string source)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(source)));
        var expression = new ExpressionSyntaxParser(context, stopOnOpenBrace: true);
        var parser = new PatternSyntaxParser(expression);
        var pattern = parser.ParsePattern().CreateRed();
        Assert.Empty(parser.Diagnostics);
        Assert.NotEqual(SyntaxKind.PropertyPattern, pattern.Kind);
        Assert.Equal("Widget", pattern.ToString());
        Assert.Equal(SyntaxKind.OpenBraceToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitPropertyPatternRetainsItsMemberClause(bool header)
    {
        const string source = "Widget { Name: not null }";
        var context = new BaseParseContext(new Lexer(new StringReader(source)));
        var parser = new PatternSyntaxParser(new ExpressionSyntaxParser(context, stopOnOpenBrace: header));
        Assert.Equal(SyntaxKind.PropertyPattern, parser.ParsePattern().Kind);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void OrdinaryPatternRetainsMissingColonRecovery()
    {
        var context = new BaseParseContext(new Lexer(new StringReader("Widget { Name }")));
        var parser = new PatternSyntaxParser(context);
        Assert.Equal(SyntaxKind.PropertyPattern, parser.ParsePattern().Kind);
        Assert.NotEmpty(parser.Diagnostics);
    }
}
