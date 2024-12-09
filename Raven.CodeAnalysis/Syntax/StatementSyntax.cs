namespace Raven.CodeAnalysis.Syntax;

public abstract class StatementSyntax : SyntaxNode
{
    protected StatementSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    protected StatementSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}