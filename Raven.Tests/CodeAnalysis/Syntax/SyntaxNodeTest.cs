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
            OpenBraceToken
                .WithLeadingTrivia(LineFeed)
                .WithTrailingTrivia(LineFeed),
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword.WithLeadingTrivia(Tab),
                    LiteralExpression(NumericLiteral(42).WithLeadingTrivia(Whitespace(" "))),
                    SemicolonToken.WithTrailingTrivia(LineFeed))
                    .WithTrailingTrivia(LineFeed)
            ),
            CloseBraceToken
                .WithTrailingTrivia(LineFeed)
        );

        var x = block.Statements[0];

        foreach (var nodeOrToken in block.ChildNodesAndTokens())
        {
            testOutputHelper.WriteLine($"{nodeOrToken.Node?.Kind ?? nodeOrToken.Token.Kind} [{nodeOrToken.Node?.FullSpan ?? nodeOrToken.Token.FullSpan}]");
        }

        var str = block.ToString();
        var str2 = block.ToFullString();

        testOutputHelper.WriteLine(str2);
    }
}