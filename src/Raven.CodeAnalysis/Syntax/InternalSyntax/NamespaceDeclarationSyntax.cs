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
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.NamespaceDeclaration,
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
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(namespaceKeyword, name, openBraceToken, imports, members, closeBraceToken, semicolonToken, diagnostics);
}