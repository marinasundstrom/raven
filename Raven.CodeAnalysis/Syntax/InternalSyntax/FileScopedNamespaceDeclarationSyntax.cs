namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members)
        : base(
              SyntaxKind.FileScopedNamespaceDeclaration,
              [
                    namespaceKeyword,
                    name,
                    semicolonToken,
                    imports,
                    members
              ])
    {
    }
}