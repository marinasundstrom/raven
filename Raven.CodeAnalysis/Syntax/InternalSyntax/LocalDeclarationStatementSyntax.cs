namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken semicolonToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                      declaration,
                      semicolonToken
              ],
              declaration.FullWidth + semicolonToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.LocalDeclarationStatementSyntax(this, parent);
    }
}