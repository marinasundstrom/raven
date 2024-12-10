namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class LiteralExpressionSyntax : ExpressionSyntax
{
    protected LiteralExpressionSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}

public class NumericLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public NumericLiteralExpressionSyntax(
        SyntaxToken numberToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.NumericLiteralExpression,
              [
                numberToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.NumericLiteralExpressionSyntax(this, parent, position);
    }
}