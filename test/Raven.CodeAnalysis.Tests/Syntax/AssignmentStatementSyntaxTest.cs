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
    public void ParsesCompoundAssignmentStatements(string source, SyntaxKind expectedKind, SyntaxKind expectedOperator)
    {
        var tree = SyntaxTree.ParseText(source);
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        Assert.Equal(expectedKind, assignment.Kind);
        Assert.Equal(expectedOperator, assignment.OperatorToken.Kind);
    }

}
