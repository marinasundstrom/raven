namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public UnaryExpressionSyntax(
        SyntaxToken operatorToken,
        ExpressionSyntax expression,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.UnaryExpression,
              [
                      operatorToken,
                      expression
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
        IEnumerable<Diagnostic>? diagnostics = null)
      => new(operatorToken, expression, diagnostics);
}