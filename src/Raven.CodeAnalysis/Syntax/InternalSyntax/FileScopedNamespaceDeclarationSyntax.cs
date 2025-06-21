namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken terminatorToken,
        SyntaxList imports,
        SyntaxList members,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.FileScopedNamespaceDeclaration,
              [
                    modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                    namespaceKeyword ?? throw new ArgumentNullException(nameof(namespaceKeyword)),
                    name ?? throw new ArgumentNullException(nameof(name)),
                    terminatorToken ?? throw new ArgumentNullException(nameof(terminatorToken)),
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
        SyntaxList modifiers,
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken terminatorToken,
        SyntaxList imports,
        SyntaxList members,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, namespaceKeyword, name, terminatorToken, imports, members, diagnostics);
}