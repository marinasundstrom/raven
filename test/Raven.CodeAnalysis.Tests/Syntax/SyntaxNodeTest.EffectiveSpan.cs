namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public partial class SyntaxNodeTest
{
    [Fact]
    public void EffectiveSpan_ExcludesNewlineTerminator()
    {
        var returnStatement = ReturnStatement(
            ReturnKeyword,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(1)),
            NewLineToken);

        var span = returnStatement.Span;
        var effectiveSpan = returnStatement.EffectiveSpan;

        effectiveSpan.End.ShouldBe(returnStatement.TerminatorToken.Span.Start);
        effectiveSpan.Length.ShouldBe(span.Length - returnStatement.TerminatorToken.Span.Length);
    }

    [Fact]
    public void EffectiveSpan_IncludesNonNewlineTerminator()
    {
        var returnStatement = ReturnStatement(
            ReturnKeyword,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(1)),
            SemicolonToken);

        returnStatement.EffectiveSpan.ShouldBe(returnStatement.Span);
    }
}
