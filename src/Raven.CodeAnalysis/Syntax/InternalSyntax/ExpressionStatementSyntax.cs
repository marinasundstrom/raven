namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(
        ExpressionSyntax expression,
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression,
                    semicolonToken,
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
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(expression, semicolonToken, diagnostics);
}