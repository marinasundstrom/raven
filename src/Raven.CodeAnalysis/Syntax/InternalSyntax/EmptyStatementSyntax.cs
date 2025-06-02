namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EmptyStatementSyntax : StatementSyntax
{
    public EmptyStatementSyntax(
        SyntaxToken terminationToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.EmptyStatement,
              [
                      terminationToken ?? throw new ArgumentNullException(nameof(terminationToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static EmptyStatementSyntax EmptyStatement(
        SyntaxToken terminationToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(terminationToken, diagnostics);
}