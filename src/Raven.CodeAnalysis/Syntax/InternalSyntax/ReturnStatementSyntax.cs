namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ReturnStatementSyntax : StatementSyntax
{
    public ReturnStatementSyntax(
        SyntaxToken returnKeyword,
        ExpressionSyntax? expression,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ReturnStatement,
              [
                      returnKeyword ?? throw new ArgumentNullException(nameof(returnKeyword)),
                      expression,
                      semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ReturnStatementSyntax ReturnStatement(
        SyntaxToken returnKeyword,
        ExpressionSyntax expression,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(returnKeyword, expression, semicolonToken, diagnostics);
}