namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                    declaration,
                    semicolonToken
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
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(declaration, semicolonToken, diagnostics);
}