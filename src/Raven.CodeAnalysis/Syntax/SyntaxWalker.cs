namespace Raven.CodeAnalysis.Syntax;

public abstract class SyntaxWalker : SyntaxVisitor
{
    private int _recursionDepth;

    public override void Visit(SyntaxNode? node)
    {
        if (node is null)
        {
            return;
        }

        _recursionDepth++;

        node.Accept(this);

        _recursionDepth--;
    }
}