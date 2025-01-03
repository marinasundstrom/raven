namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseNamespaceDeclarationSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}