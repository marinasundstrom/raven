namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public UnaryExpressionSyntax(
        SyntaxKind kind,
        SyntaxToken operatorToken,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              kind,
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
        SyntaxKind kind,
        SyntaxToken operatorToken,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(kind, operatorToken, expression, diagnostics);
}