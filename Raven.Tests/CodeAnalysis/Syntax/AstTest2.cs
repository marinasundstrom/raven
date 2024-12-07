using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class AstTest2
{
    [Fact]
    public void Test2()
    {
        var condition = new BinaryExpressionSyntax(
            new IdentifierSyntax(Identifier("x")),
            GreaterThanToken,
            new IdentifierSyntax(Identifier("y")));

        var foo = new IfStatementSyntax(condition, new BlockSyntax(SyntaxList<StatementSyntax>.Empty));

        var s = foo.Statement;

        var x = foo.ElseClause;

        var c = foo.ChildNodesAndTokens();
        foreach (var e in c)
        {

        }

        var f = foo.TrailingTrivia;
    }
}