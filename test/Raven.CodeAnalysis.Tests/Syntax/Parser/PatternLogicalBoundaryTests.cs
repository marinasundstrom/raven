using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class PatternLogicalBoundaryTests
{
    [Theory]
    [InlineData("1", "&&", SyntaxKind.ConstantPattern)]
    [InlineData("1", "||", SyntaxKind.ConstantPattern)]
    [InlineData("'('", "&&", SyntaxKind.ConstantPattern)]
    [InlineData("'(' or '[' or '{'", "&&", SyntaxKind.OrPattern)]
    [InlineData("1 or 2", "||", SyntaxKind.OrPattern)]
    [InlineData("One", "&&", SyntaxKind.ConstantPattern)]
    [InlineData("Limits.One", "||", SyntaxKind.ConstantPattern)]
    [InlineData("> 1", "&&", SyntaxKind.GreaterThanPattern)]
    [InlineData("1..10", "||", SyntaxKind.RangePattern)]
    [InlineData("not null", "&&", SyntaxKind.NotPattern)]
    public void LogicalOperatorBelongsToEnclosingExpression(string pattern, string logicalOperator, SyntaxKind patternKind)
    {
        var source = $"class Probe {{ static func Test(value: int, flag: bool) -> bool => value is {pattern} {logicalOperator} flag }}";
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        var logical = Assert.Single(tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>()
            .Where(n => n.OperatorToken.Text == logicalOperator));
        var test = Assert.IsType<IsPatternExpressionSyntax>(logical.Left);
        Assert.Equal(patternKind, test.Pattern.Kind);
        Assert.Equal(pattern, test.Pattern.ToString());
        Assert.Equal("flag", Assert.IsType<IdentifierNameSyntax>(logical.Right).Identifier.Text);
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void UnparenthesizedCharacterPatternRetainsControlFlowBlock()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Test(ch: char) -> int {
                    var depth = 0
                    if ch is '(' or '[' or '{' && ++depth > 128 { return 1 }
                    return depth
                }
            }
            """);
        Assert.Empty(tree.GetDiagnostics());
        var logical = Assert.Single(tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>()
            .Where(n => n.Kind == SyntaxKind.LogicalAndExpression));
        Assert.IsType<IsPatternExpressionSyntax>(logical.Left);
        Assert.Equal(SyntaxKind.GreaterThanExpression, logical.Right.Kind);
    }
}
