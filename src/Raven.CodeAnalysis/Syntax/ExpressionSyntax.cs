namespace Raven.CodeAnalysis.Syntax;

public abstract class ExpressionSyntax : SyntaxNode
{
    internal ExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}