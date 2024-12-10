namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                    namespaceKeyword,
                    name,
                    imports,
                    members,
                    semicolonToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.FileScopedNamespaceDeclarationSyntax(this, parent, position);
    }
}