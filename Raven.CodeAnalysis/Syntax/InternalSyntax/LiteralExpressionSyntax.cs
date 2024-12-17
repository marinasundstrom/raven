namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LiteralExpressionSyntax : ExpressionSyntax
{
    public LiteralExpressionSyntax(SyntaxKind kind, SyntaxToken token)
        : base(kind, [token])
    {
    }
}