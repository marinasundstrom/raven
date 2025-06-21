namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public NamespaceDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.NamespaceDeclaration,
              [
                    modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                    namespaceKeyword ?? throw new ArgumentNullException(nameof(namespaceKeyword)),
                    name ?? throw new ArgumentNullException(nameof(name)),
                    openBraceToken ?? throw new ArgumentNullException(nameof(openBraceToken)),
                    imports ?? throw new ArgumentNullException(nameof(imports)),
                    members ?? throw new ArgumentNullException(nameof(members)),
                    closeBraceToken ?? throw new ArgumentNullException(nameof(closeBraceToken)),
                    terminatorToken ?? throw new ArgumentNullException(nameof(terminatorToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static NamespaceDeclarationSyntax NamespaceDeclaration(
        SyntaxList modifiers,
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, namespaceKeyword, name, openBraceToken, imports, members, closeBraceToken, terminatorToken, diagnostics);
}