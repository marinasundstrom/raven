namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public NamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken semicolonToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                    namespaceKeyword,
                    name,
                    openBraceToken,
                    imports,
                    members,
                    closeBraceToken,
                    semicolonToken
              ],
              (imports?.FullWidth ?? 0) + (members?.FullWidth ?? 0),
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.NamespaceDeclarationSyntax(this, parent);
    }
}