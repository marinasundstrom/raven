namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                    declaration ?? throw new ArgumentNullException(nameof(declaration)),
                    terminatorToken ?? throw new ArgumentNullException(nameof(terminatorToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(
        VariableDeclarationSyntax declaration,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(declaration, terminatorToken, diagnostics);
}