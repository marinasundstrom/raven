namespace Raven.CodeAnalysis.Syntax;

public abstract class ExpressionSyntax : SyntaxNode
{
    protected ExpressionSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    protected ExpressionSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}