namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      leftHandSide,
                      operatorToken,
                      rightHandSide
              ],
              leftHandSide.FullWidth + (operatorToken?.FullWidth ?? 0) + rightHandSide.FullWidth,
              diagnostics,
              startPosition)
    {

    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.BinaryExpressionSyntax(this, parent);
    }
}