using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxNodeTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test1()
    {
        var block = Block(
            List<StatementSyntax>(
                ReturnStatement(LiteralExpression(20))
            )
        );

        var x = block.Statements[0];

        foreach (var nodeOrToken in block.ChildNodesAndTokens())
        {
            testOutputHelper.WriteLine($"{nodeOrToken.Node?.Kind ?? nodeOrToken.Token.Kind} [{nodeOrToken.Node?.FullSpan ?? nodeOrToken.Token.FullSpan}]");
        }
    }
}