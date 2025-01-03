namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.FileScopedNamespaceDeclaration,
              [
                    namespaceKeyword,
                    name,
                    semicolonToken,
                    imports,
                    members
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(namespaceKeyword, name, semicolonToken, imports, members, diagnostics);
}