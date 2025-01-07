namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.FileScopedNamespaceDeclaration,
              [
                    namespaceKeyword ?? throw new ArgumentNullException(nameof(namespaceKeyword)),
                    name ?? throw new ArgumentNullException(nameof(name)),
                    semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken)),
                    imports ?? throw new ArgumentNullException(nameof(imports)),
                    members ?? throw new ArgumentNullException(nameof(members))
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
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(namespaceKeyword, name, semicolonToken, imports, members, diagnostics);
}