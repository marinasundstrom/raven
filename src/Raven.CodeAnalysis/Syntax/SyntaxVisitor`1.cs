namespace Raven.CodeAnalysis.Syntax;

public abstract partial class SyntaxVisitor<TResult>
{
    public virtual TResult? Visit(SyntaxNode? node)
    {
        if (node is not null)
        {
            return node.Accept(this);
        }

        // should not come here too often so we will put this at the end of the method.
        return default;
    }

    public virtual TResult DefaultVisit(SyntaxNode node)
    {
        return default!;
    }

    // WORKAROUND

    public TResult VisitPattern(PatternSyntax node)
    {
        return default!;

    }

    public TResult VisitVariableDesignation(VariableDesignationSyntax node)
    {
        return default!;
    }
}