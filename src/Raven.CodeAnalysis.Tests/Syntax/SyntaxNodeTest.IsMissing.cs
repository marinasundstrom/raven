namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public partial class SyntaxNodeTest
{
    [Fact]
    public void ReturnStatement_AllChildrenMissing_ShouldBeMarkedAsMissing()
    {
        var block = ReturnStatement(MissingToken(SyntaxKind.ReturnKeyword), MissingToken(SyntaxKind.SemicolonToken));

        block.IsMissing.ShouldBe(true);

        testOutputHelper.WriteLine(block.ToFullString());
    }

    [Fact]
    public void Block_AllChildrenMissing_ShouldBeMarkedAsMissing()
    {
        var block = Block(
            MissingToken(SyntaxKind.OpenBraceToken),
            List<StatementSyntax>(),
            MissingToken(SyntaxKind.CloseBraceToken));

        block.IsMissing.ShouldBe(true);

        testOutputHelper.WriteLine(block.ToFullString());
    }

    [Fact]
    public void Block_AllChildrenMissingIncludingNestedNode_ShouldBeMarkedAsMissing()
    {
        var block = Block(
            MissingToken(SyntaxKind.OpenBraceToken),
             List<StatementSyntax>(
                ReturnStatement(MissingToken(SyntaxKind.ReturnKeyword), MissingToken(SyntaxKind.SemicolonToken))
            ),
            MissingToken(SyntaxKind.CloseBraceToken));

        block.IsMissing.ShouldBe(true);

        testOutputHelper.WriteLine(block.ToFullString());
    }

    [Fact]
    public void Block_SomeChildrenMissing_ShouldNotBeMarkedAsMissing()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                ReturnStatement(MissingToken(SyntaxKind.ReturnKeyword), MissingToken(SyntaxKind.SemicolonToken))
            ),
            MissingToken(SyntaxKind.CloseBraceToken));

        block.IsMissing.ShouldBe(false);

        testOutputHelper.WriteLine(block.ToFullString());
    }

    [Fact]
    public void Block_WithValidTokenAndSomeMissingChildren_ShouldNotBeMarkedAsMissing()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword, null, MissingToken(SyntaxKind.SemicolonToken))
            ),
            MissingToken(SyntaxKind.CloseBraceToken));

        block.IsMissing.ShouldBe(false);

        testOutputHelper.WriteLine(block.ToFullString());
    }
}