namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public NamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                    namespaceKeyword,
                    name,
                    openBraceToken,
                    imports,
                    members,
                    closeBraceToken,
                    semicolonToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.NamespaceDeclarationSyntax(this, parent, position);
    }
}