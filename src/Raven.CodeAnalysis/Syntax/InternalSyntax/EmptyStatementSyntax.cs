namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EmptyStatementSyntax : StatementSyntax
{
    public EmptyStatementSyntax(
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.EmptyStatement,
              [
                      semicolonToken
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static EmptyStatementSyntax EmptyStatement(
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(semicolonToken, diagnostics);
}