
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class TypeSyntax : SyntaxNode
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots, int fullWidth, IEnumerable<DiagnosticInfo> diagnostics = null, int startPosition = 0)
        : base(kind, slots, fullWidth, diagnostics, startPosition)
    {
    }
}


public class NameSyntax : TypeSyntax
{
    public NameSyntax(SyntaxToken nameToken)
        : base(SyntaxKind.QualifiedName, [nameToken], nameToken.FullWidth, [], 0)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.NameSyntax(this, parent);
    }
}