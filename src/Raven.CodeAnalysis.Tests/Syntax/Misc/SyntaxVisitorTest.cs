namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxVisitorTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test1()
    {
        var ifStatement = IfStatement(
                condition: BinaryExpression(
                    SyntaxKind.GreaterThanExpression,
                    IdentifierName("x"),
                    GreaterThanToken,
                    IdentifierName("y")),
                statement: Block(SingletonList<StatementSyntax>(
                    ReturnStatement(
                        LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(2)))
                )));
        /*
        var visitor = new TestSyntaxVisitor();
        
        visitor.Visit(ifStatement);

        var visitor2 = new TestSyntaxVisitor2();

        var r = visitor2.Visit(ifStatement); */

        var rewriter = new SyntaxNormalizer();

        var newTree = rewriter.Visit(ifStatement);

        testOutputHelper.WriteLine(newTree.ToFullString());
    }
}
