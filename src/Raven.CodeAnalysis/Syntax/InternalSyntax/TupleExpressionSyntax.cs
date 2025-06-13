namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TupleExpressionSyntax : ExpressionSyntax
{
    public TupleExpressionSyntax(
        SyntaxToken openParenToken,
        SyntaxList arguments,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.TupleExpression,
              [
                      openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                      arguments ?? throw new ArgumentNullException(nameof(arguments)),
                      closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static TupleExpressionSyntax TupleExpression(
            SyntaxToken openParenToken,
            SyntaxList arguments,
            SyntaxToken closeParentToken,
            IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openParenToken, arguments, closeParentToken, diagnostics);
}