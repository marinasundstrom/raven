
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class MemberDeclarationSyntax : SyntaxNode
{
    protected MemberDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo> diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}