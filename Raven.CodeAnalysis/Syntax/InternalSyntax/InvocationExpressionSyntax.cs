
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class InvocationExpressionSyntax : ExpressionSyntax
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