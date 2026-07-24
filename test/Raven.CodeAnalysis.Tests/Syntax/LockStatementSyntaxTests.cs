using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class LockStatementSyntaxTests
{
    [Fact]
    public void LockStatement_ParsesExpressionAndBody()
    {
        const string testCode = """
lock gate {
    Work()
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var lockStatement = Assert.IsType<LockStatementSyntax>(statement);

        lockStatement.LockKeyword.Kind.ShouldBe(SyntaxKind.LockKeyword);
        lockStatement.Expression.ShouldBeOfType<IdentifierNameSyntax>();
        lockStatement.Statement.ShouldBeOfType<BlockStatementSyntax>();
        tree.GetDiagnostics().ShouldBeEmpty();
    }
}
