
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class InvocationExpressionSyntax : ExpressionSyntax
{
    public InvocationExpressionSyntax(
        ExpressionSyntax expression,
        ArgumentListSyntax argumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
            SyntaxKind.InvocationExpression,
              [
                      expression ?? throw new ArgumentNullException(nameof(expression)),
                      argumentList ?? throw new ArgumentNullException(nameof(argumentList)),
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static InvocationExpressionSyntax InvocationExpression(
        ExpressionSyntax expression,
        ArgumentListSyntax argumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(expression, argumentList, diagnostics);
}