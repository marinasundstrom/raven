namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(
        ExpressionSyntax expression,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression ?? throw new ArgumentNullException(nameof(expression)),
                    semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(
        ExpressionSyntax expression,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(expression, semicolonToken, diagnostics);
}