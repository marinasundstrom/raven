
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ElementAccessExpressionSyntax : ExpressionSyntax
{
    public ElementAccessExpressionSyntax(
        ExpressionSyntax expression,
        BracketedArgumentListSyntax argumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
            SyntaxKind.ElementAccessExpression,
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
    public static ElementAccessExpressionSyntax ElementAccessExpression(
        ExpressionSyntax expression,
        BracketedArgumentListSyntax argumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(expression, argumentList, diagnostics);
}