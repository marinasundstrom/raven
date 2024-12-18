namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        NameSyntax name,
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