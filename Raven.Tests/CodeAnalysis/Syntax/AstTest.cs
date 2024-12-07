using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class AstTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test1()
    {
        var ifStatement = IfStatement(
                condition: BinaryExpression(
                    IdentifierName("x"),
                    GreaterThanToken,
                    IdentifierName("y")),
                statement: Block(SingletonList<StatementSyntax>(
                    ReturnStatement(
                        LiteralExpression(2))
                )));

        var ifStatementWithElseClause = ifStatement
                .WithElseClause(
                    ElseClause(
                        ReturnStatement(
                            LiteralExpression(2))));

        testOutputHelper.WriteLine($"Equal: {ifStatement} == {ifStatementWithElseClause} = {ifStatement == ifStatementWithElseClause}");

        var desc = ifStatementWithElseClause.Descendants().ToArray();

        var returns = ifStatementWithElseClause.Descendants().OfType<ReturnStatementSyntax>().ToArray();

        var x2 = returns.First().Ancestors().OfType<IfStatementSyntax>().ToList();

        var s = ifStatementWithElseClause.Statement;

        var x = ifStatementWithElseClause.ElseClause;

        var c = ifStatementWithElseClause.ChildNodesAndTokens();
        foreach (var e in c)
        {
            testOutputHelper.WriteLine($"{e.Node?.Kind ?? e.Token.Kind}");
        }

        /*
        var c2 = foo2.ChildNodes();
        foreach (var e in c2)
        {
            testOutputHelper.WriteLine($"{e}");
        }

        var ancestors = foo2.Ancestors();
        foreach (var e in ancestors)
        {
            testOutputHelper.WriteLine($"{e}");
        }

        var descendants = foo2.Descendants();
        foreach (var e in descendants)
        {
            testOutputHelper.WriteLine($"{e}");
        }
        */

        var f = ifStatementWithElseClause.TrailingTrivia;
    }
}