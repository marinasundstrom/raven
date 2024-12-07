namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class LiteralExpressionSyntax : ExpressionSyntax
{
    protected LiteralExpressionSyntax(SyntaxKind kind, GreenNode[] slots, int fullWidth, IEnumerable<DiagnosticInfo> diagnostics = null, int startPosition = 0) : base(kind, slots, fullWidth, diagnostics, startPosition)
    {
    }
}

public class NumericLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public NumericLiteralExpressionSyntax(
        SyntaxToken numberToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.NumericLiteralExpression,
              [
                numberToken
              ],
              numberToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.NumericLiteralExpressionSyntax(this, parent);
    }
}
