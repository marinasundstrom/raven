namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseNamespaceDeclarationSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}