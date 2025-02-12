namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public UnaryExpressionSyntax(
        SyntaxToken operatorToken,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.UnaryExpression,
              [
                      operatorToken ?? throw new ArgumentNullException(nameof(operatorToken)),
                      expression ?? throw new ArgumentNullException(nameof(expression))
              ],
              diagnostics)
    {

    }
}

internal static partial class SyntaxFactory
{
    public static UnaryExpressionSyntax UnaryExpression(
        SyntaxToken operatorToken,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(operatorToken, expression, diagnostics);
}