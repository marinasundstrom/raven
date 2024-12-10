namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                      declaration,
                      semicolonToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.LocalDeclarationStatementSyntax(this, parent, position);
    }
}