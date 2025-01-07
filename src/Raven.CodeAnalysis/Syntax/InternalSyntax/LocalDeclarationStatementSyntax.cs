namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                    declaration ?? throw new ArgumentNullException(nameof(declaration)),
                    semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(
        VariableDeclarationSyntax declaration,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(declaration, semicolonToken, diagnostics);
}