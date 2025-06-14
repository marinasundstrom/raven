namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ReturnStatementSyntax : StatementSyntax
{
    public ReturnStatementSyntax(
        SyntaxToken returnKeyword,
        ExpressionSyntax? expression,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ReturnStatement,
              [
                      returnKeyword ?? throw new ArgumentNullException(nameof(returnKeyword)),
                      expression,
                      terminatorToken ?? throw new ArgumentNullException(nameof(terminatorToken))
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
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(returnKeyword, expression, terminatorToken, diagnostics);
}