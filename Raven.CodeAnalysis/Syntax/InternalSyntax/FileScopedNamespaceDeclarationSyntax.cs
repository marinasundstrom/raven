namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members)
        : base(
              SyntaxKind.CompilationUnit,
              [
                    namespaceKeyword,
                    name,
                    imports,
                    members,
                    semicolonToken
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.FileScopedNamespaceDeclarationSyntax(this, parent, position);
    }
}