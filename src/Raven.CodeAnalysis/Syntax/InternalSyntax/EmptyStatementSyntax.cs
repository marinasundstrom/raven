namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EmptyStatementSyntax : StatementSyntax
{
    public EmptyStatementSyntax(
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.EmptyStatement,
              [
                      semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static EmptyStatementSyntax EmptyStatement(
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(semicolonToken, diagnostics);
}