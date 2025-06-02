namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public NamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken terminationToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.NamespaceDeclaration,
              [
                    namespaceKeyword ?? throw new ArgumentNullException(nameof(namespaceKeyword)),
                    name ?? throw new ArgumentNullException(nameof(name)),
                    openBraceToken ?? throw new ArgumentNullException(nameof(openBraceToken)),
                    imports ?? throw new ArgumentNullException(nameof(imports)),
                    members ?? throw new ArgumentNullException(nameof(members)),
                    closeBraceToken ?? throw new ArgumentNullException(nameof(closeBraceToken)),
                    terminationToken ?? throw new ArgumentNullException(nameof(terminationToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static NamespaceDeclarationSyntax NamespaceDeclaration(
        SyntaxToken namespaceKeyword,
        NameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken terminationToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(namespaceKeyword, name, openBraceToken, imports, members, closeBraceToken, terminationToken, diagnostics);
}