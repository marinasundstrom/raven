namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParenthesizedExpressionSyntax : ExpressionSyntax
{
    public ParenthesizedExpressionSyntax(
        SyntaxToken openParenToken,
        ExpressionSyntax expression,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ParenthesizedExpression,
              [
                      openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                      expression ?? throw new ArgumentNullException(nameof(expression)),
                      closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken))
              ],
              diagnostics)
    {

    }
}

internal static partial class SyntaxFactory
{
    public static ParenthesizedExpressionSyntax ParenthesizedExpression(
        SyntaxToken openParenToken,
        ExpressionSyntax expression,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openParenToken, expression, closeParenToken, diagnostics);
}