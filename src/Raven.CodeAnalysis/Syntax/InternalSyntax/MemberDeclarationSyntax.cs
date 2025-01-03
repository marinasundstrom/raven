
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class MemberDeclarationSyntax : SyntaxNode
{
    protected MemberDeclarationSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}