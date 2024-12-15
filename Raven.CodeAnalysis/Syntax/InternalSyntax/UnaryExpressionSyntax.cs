namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public UnaryExpressionSyntax(
        SyntaxToken operatorToken,
        ExpressionSyntax expression)
        : base(
              SyntaxKind.UnaryExpression,
              [
                      operatorToken,
                      expression
              ])
    {

    }
}