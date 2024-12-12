namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide)
        : base(
              SyntaxKind.Block,
              [
                      leftHandSide,
                      operatorToken,
                      rightHandSide
              ])
    {

    }
}