namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      leftHandSide,
                      operatorToken,
                      rightHandSide
              ],
              diagnostics)
    {

    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.BinaryExpressionSyntax(this, parent, position);
    }
}