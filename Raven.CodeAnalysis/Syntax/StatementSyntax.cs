namespace Raven.CodeAnalysis.Syntax;

public abstract class StatementSyntax : SyntaxNode
{
    protected StatementSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}