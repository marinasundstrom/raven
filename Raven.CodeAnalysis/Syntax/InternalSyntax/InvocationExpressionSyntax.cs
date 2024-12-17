
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class InvocationExpressionSyntax : ExpressionSyntax
{
    public InvocationExpressionSyntax(ExpressionSyntax expression, ArgumentListSyntax argumentList)
        : base(SyntaxKind.InvocationExpression,
              [
                      expression,
                      argumentList,
              ])
    {
    }
}