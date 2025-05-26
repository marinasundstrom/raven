namespace Raven.CodeAnalysis.Syntax;

public abstract class ExpressionOrPatternSyntax : SyntaxNode
{
    internal ExpressionOrPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}