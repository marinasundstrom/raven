namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ParenthesizedExpressionSyntax : ExpressionSyntax
{
    public ParenthesizedExpressionSyntax(
        SyntaxToken openParenToken,
        ExpressionSyntax expression,
        SyntaxToken closeParenToken)
        : base(
              SyntaxKind.ParenthesizedExpression,
              [
                      openParenToken,
                      expression,
                      closeParenToken
              ])
    {

    }
}
