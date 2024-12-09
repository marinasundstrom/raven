namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseNamespaceDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, int fullWidth, IEnumerable<DiagnosticInfo> diagnostics = null, int startPosition = 0) : base(kind, slots, fullWidth, diagnostics, startPosition)
    {
    }
}
