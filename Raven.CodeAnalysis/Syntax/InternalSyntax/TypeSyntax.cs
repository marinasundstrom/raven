
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class TypeSyntax : SyntaxNode
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}


public class NameSyntax : TypeSyntax
{
    public NameSyntax(SyntaxToken nameToken)
        : base(SyntaxKind.QualifiedName, [nameToken], [])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.NameSyntax(this, parent, position);
    }
}