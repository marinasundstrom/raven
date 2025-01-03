namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParenthesizedExpressionSyntax : ExpressionSyntax
{
    public ParenthesizedExpressionSyntax(
        SyntaxToken openParenToken,
        ExpressionSyntax expression,
        SyntaxToken closeParenToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.ParenthesizedExpression,
              [
                      openParenToken,
                      expression,
                      closeParenToken
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
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(openParenToken, expression, closeParenToken, diagnostics);
}
