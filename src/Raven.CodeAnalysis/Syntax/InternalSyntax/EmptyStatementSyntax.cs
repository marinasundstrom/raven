namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EmptyStatementSyntax : StatementSyntax
{
    public EmptyStatementSyntax(
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.EmptyStatement,
              [
                      terminatorToken ?? throw new ArgumentNullException(nameof(terminatorToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static EmptyStatementSyntax EmptyStatement(
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(terminatorToken, diagnostics);
}