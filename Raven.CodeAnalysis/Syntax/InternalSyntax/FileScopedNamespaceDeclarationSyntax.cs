namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public FileScopedNamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken semicolonToken,
        SyntaxList imports,
        SyntaxList members,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                    namespaceKeyword,
                    name,
                    imports,
                    members,
                    semicolonToken
              ],
              (imports?.FullWidth ?? 0) + (members?.FullWidth ?? 0),
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.FileScopedNamespaceDeclarationSyntax(this, parent);
    }
}
