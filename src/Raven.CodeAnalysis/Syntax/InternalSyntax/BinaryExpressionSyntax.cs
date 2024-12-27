namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        SyntaxKind kind,
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide)
        : base(
              kind,
              [
                      leftHandSide,
                      operatorToken,
                      rightHandSide
              ])
    {

    }
}
