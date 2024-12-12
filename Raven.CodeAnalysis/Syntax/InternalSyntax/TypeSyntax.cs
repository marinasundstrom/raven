
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class TypeSyntax : SyntaxNode
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}


public partial class NameSyntax : TypeSyntax
{
    public NameSyntax(SyntaxToken nameToken)
        : base(SyntaxKind.QualifiedName, [nameToken])
    {
    }
}