using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class AstTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test1()
    {
        var foo = IfStatement(
                condition: BinaryExpression(
                    IdentifierName("x"),
                    GreaterThanToken,
                    IdentifierName("y")),
                statement: Block(SingletonList<StatementSyntax>(
                    ReturnStatement(
                        LiteralExpression(2))
                )))
                .WithElseClause(
                    ElseClause(
                        ReturnStatement(
                            LiteralExpression(2))));

        var desc = foo.Descendants().ToArray();

        var returns = foo.Descendants().OfType<ReturnStatementSyntax>().ToArray();

        var s = foo.Statement;

        var x = foo.ElseClause;

        var c = foo.ChildNodesAndTokens();
        foreach (var e in c)
        {
            testOutputHelper.WriteLine($"{e.Node?.Kind ?? e.Token.Kind}");
        }

        /*
        var c2 = foo.ChildNodes();
        foreach (var e in c2)
        {
            testOutputHelper.WriteLine($"{e}");
        }

        var ancestors = foo.Ancestors();
        foreach (var e in ancestors)
        {
            testOutputHelper.WriteLine($"{e}");
        }

        var descendants = foo.Descendants();
        foreach (var e in descendants)
        {
            testOutputHelper.WriteLine($"{e}");
        }
        */

        var f = foo.TrailingTrivia;
    }
}