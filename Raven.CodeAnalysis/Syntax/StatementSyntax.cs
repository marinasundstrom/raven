namespace Raven.CodeAnalysis.Syntax;

public abstract class StatementSyntax : SyntaxNode
{
    internal StatementSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}