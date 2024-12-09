namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ParameterSyntax : SyntaxNode
{
    public ParameterSyntax()
        : base(SyntaxKind.Parameter, [], 0, [], 0)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ParameterSyntax(this, parent);
    }
}