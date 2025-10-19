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

    [Fact]
    public void ParsesDiscardAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("_ = 1");
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.DiscardAssignmentStatement, assignment.Kind);
        Assert.True(assignment.IsDiscard);
        Assert.IsType<DiscardExpressionSyntax>(assignment.Left);
    }
}
