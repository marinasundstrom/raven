namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class LiteralExpressionSyntax : ExpressionSyntax
{
    public LiteralExpressionSyntax(SyntaxKind kind, SyntaxToken token)
        : base(kind, [token])
    {
    }
}