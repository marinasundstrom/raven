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

}
