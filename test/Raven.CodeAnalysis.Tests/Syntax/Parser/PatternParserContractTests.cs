using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class PatternParserContractTests
{
    [Theory]
    [InlineData("_", SyntaxKind.DiscardPattern)]
    [InlineData("null", SyntaxKind.ConstantPattern)]
    [InlineData("true", SyntaxKind.ConstantPattern)]
    [InlineData("-12", SyntaxKind.ConstantPattern)]
    [InlineData("\"hello\"", SyntaxKind.ConstantPattern)]
    [InlineData("int value", SyntaxKind.DeclarationPattern)]
    [InlineData("(int) -> int callback", SyntaxKind.DeclarationPattern)]
    [InlineData("val value", SyntaxKind.VariablePattern)]
    [InlineData("var (first, second)", SyntaxKind.VariablePattern)]
    [InlineData("let value: int", SyntaxKind.VariablePattern)]
    [InlineData("1 or 2", SyntaxKind.OrPattern)]
    [InlineData("1 | 2", SyntaxKind.OrPattern)]
    [InlineData("> 1 and < 10", SyntaxKind.AndPattern)]
    [InlineData("> 1 or < -10", SyntaxKind.OrPattern)]
    [InlineData("> 1 | < -10", SyntaxKind.OrPattern)]
    [InlineData("1..10 and not 5", SyntaxKind.AndPattern)]
    [InlineData("minimum or maximum", SyntaxKind.OrPattern)]
    [InlineData("> (1 | 2) and < 10", SyntaxKind.AndPattern)]
    [InlineData("not null", SyntaxKind.NotPattern)]
    [InlineData("(1 or 2)", SyntaxKind.ParenthesizedPattern)]
    [InlineData("(1, 2)", SyntaxKind.PositionalPattern)]
    [InlineData("(1,) whole", SyntaxKind.PositionalPattern)]
    [InlineData("Point(1, 2)", SyntaxKind.NominalDeconstructionPattern)]
    [InlineData("{ Length: > 0 }", SyntaxKind.PropertyPattern)]
    [InlineData("Point { Position.X: 1 }", SyntaxKind.PropertyPattern)]
    [InlineData("[1, ...rest]", SyntaxKind.SequencePattern)]
    [InlineData("[\"key\": 1]", SyntaxKind.DictionaryPattern)]
    [InlineData(".Some(val value)", SyntaxKind.MemberPattern)]
    [InlineData("1..10", SyntaxKind.RangePattern)]
    [InlineData("..=10", SyntaxKind.RangePattern)]
    [InlineData("1..", SyntaxKind.RangePattern)]
    [InlineData("val value when > 0", SyntaxKind.GuardedPattern)]
    public void PatternPreservesSourceAndShape(string source, SyntaxKind kind)
    {
        var parser = Parser(source);
        var result = parser.ParsePattern().CreateRed();
        Assert.Equal(kind, result.Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("(first, second)")]
    [InlineData("(first: int, second: string)")]
    [InlineData("(Name: value, Age: age)")]
    [InlineData("[first, second]")]
    public void ImplicitBindingsRemainVariablePatterns(string source)
    {
        var parser = Parser(source, implicitBindings: true);
        var result = parser.ParsePattern().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(2, result.DescendantNodes().OfType<VariablePatternSyntax>().Count());
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("(1, 2")]
    [InlineData("[1, ...rest")]
    [InlineData("Point(1")]
    [InlineData(".Some(val value")]
    [InlineData("var (first, second")]
    [InlineData("[\"key\": 1")]
    [InlineData("..=")]
    public void IncompletePatternRetainsSourceAndReportsDiagnostics(string source)
    {
        var parser = Parser(source);
        var result = parser.ParsePattern().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.NotEmpty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void LogicalOperatorsRetainPrecedence()
    {
        var parser = Parser("1 or 2 and not 3");
        var root = Assert.IsType<BinaryPatternSyntax>(parser.ParsePattern().CreateRed());
        Assert.Equal(SyntaxKind.OrPattern, root.Kind);
        var right = Assert.IsType<BinaryPatternSyntax>(root.Right);
        Assert.Equal(SyntaxKind.AndPattern, right.Kind);
        Assert.Equal(SyntaxKind.NotPattern, right.Right.Kind);
    }

    [Fact]
    public void DisabledTopLevelGuardLeavesWhenForCaller()
    {
        var parser = Parser("val value when > 0");
        Assert.Equal(SyntaxKind.VariablePattern, parser.ParsePatternWithoutTopLevelGuard().Kind);
        Assert.Equal(SyntaxKind.WhenKeyword, parser.PeekToken().Kind);
    }

    private static PatternSyntaxParser Parser(string source, bool implicitBindings = false) =>
        new(new BaseParseContext(new Lexer(new StringReader(source))),
            allowImplicitDeconstructionElementBindings: implicitBindings);
}
