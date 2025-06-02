namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken terminationToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                    declaration ?? throw new ArgumentNullException(nameof(declaration)),
                    terminationToken ?? throw new ArgumentNullException(nameof(terminationToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(
        VariableDeclarationSyntax declaration,
        SyntaxToken terminationToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(declaration, terminationToken, diagnostics);
}