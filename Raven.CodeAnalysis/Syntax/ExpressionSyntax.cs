namespace Raven.CodeAnalysis.Syntax;

public abstract class ExpressionSyntax : SyntaxNode
{
    protected ExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}