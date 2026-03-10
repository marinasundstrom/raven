using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class AssignmentStatementSyntaxTest
{
    [Fact]
    public void ParsesAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("x = 1");
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.SimpleAssignmentStatement, assignment.Kind);
        Assert.False(assignment.IsDiscard);
    }

    [Theory]
    [InlineData("x += 1", SyntaxKind.AddAssignmentStatement, SyntaxKind.PlusEqualsToken)]
    [InlineData("x -= 1", SyntaxKind.SubtractAssignmentStatement, SyntaxKind.MinusEqualsToken)]
    [InlineData("x *= 1", SyntaxKind.MultiplyAssignmentStatement, SyntaxKind.StarEqualsToken)]
    [InlineData("x /= 1", SyntaxKind.DivideAssignmentStatement, SyntaxKind.SlashEqualsToken)]
    [InlineData("x &= 1", SyntaxKind.BitwiseAndAssignmentStatement, SyntaxKind.AmpersandEqualsToken)]
    [InlineData("x |= 1", SyntaxKind.BitwiseOrAssignmentStatement, SyntaxKind.BarEqualsToken)]
    [InlineData("x ^= 1", SyntaxKind.BitwiseXorAssignmentStatement, SyntaxKind.CaretEqualsToken)]
    [InlineData("x ??= 1", SyntaxKind.NullCoalesceAssignmentStatement, SyntaxKind.QuestionQuestionEqualsToken)]
    public void ParsesCompoundAssignmentStatements(string source, SyntaxKind expectedKind, SyntaxKind expectedOperator)
    {
        var tree = SyntaxTree.ParseText(source);
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        Assert.Equal(expectedKind, assignment.Kind);
        Assert.Equal(expectedOperator, assignment.OperatorToken.Kind);
    }

    [Fact]
    public void ParsesCollectionPatternAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("[val first, val second, _] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.SimpleAssignmentStatement, assignment.Kind);

        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenBracketToken, pattern.OpenParenToken.Kind);
        Assert.Equal(3, pattern.Elements.Count);
    }

    [Fact]
    public void ParsesCollectionPatternDeclarationShorthandAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("val [first, second, _] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);
        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenBracketToken, pattern.OpenParenToken.Kind);
        Assert.Equal(3, pattern.Elements.Count);

        Assert.True(pattern.Elements[0].Pattern is ConstantPatternSyntax or DeclarationPatternSyntax);
        Assert.True(pattern.Elements[1].Pattern is ConstantPatternSyntax or DeclarationPatternSyntax);
        Assert.IsType<DiscardPatternSyntax>(pattern.Elements[2].Pattern);
    }

    [Fact]
    public void ParsesCollectionPatternDeclarationShorthand_WithMiddleRest()
    {
        var tree = SyntaxTree.ParseText("val [first, ..middle, last] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);
        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);
        Assert.Equal(3, pattern.Elements.Count);

        Assert.True(pattern.Elements[0].Pattern is ConstantPatternSyntax or DeclarationPatternSyntax);

        var restElement = pattern.Elements[1];
        Assert.NotNull(restElement.NameColon);
        Assert.Equal(SyntaxKind.DotDotToken, restElement.NameColon!.ColonToken.Kind);
        var middle = Assert.IsType<VariablePatternSyntax>(restElement.Pattern);
        Assert.Equal(SyntaxKind.None, middle.BindingKeyword.Kind);

        Assert.True(pattern.Elements[2].Pattern is ConstantPatternSyntax or DeclarationPatternSyntax);
    }

    [Fact]
    public void ParsesPositionalPatternDeclarationShorthandAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("var (first, second, _) = tuple");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.VarKeyword, assignment.BindingKeyword.Kind);

        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenParenToken, pattern.OpenParenToken.Kind);
        Assert.Equal(3, pattern.Elements.Count);
    }
}
