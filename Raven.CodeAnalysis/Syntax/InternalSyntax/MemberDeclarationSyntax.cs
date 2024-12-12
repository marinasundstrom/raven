
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class MemberDeclarationSyntax : SyntaxNode
{
    protected MemberDeclarationSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}