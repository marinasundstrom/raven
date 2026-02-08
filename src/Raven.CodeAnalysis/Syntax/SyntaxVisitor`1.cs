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

    public virtual TResult? VisitPattern(PatternSyntax? node)
    {
        return Visit(node);
    }

    public virtual TResult? VisitVariableDesignation(VariableDesignationSyntax? node)
    {
        return Visit(node);
    }

    public virtual TResult? VisitExpressionOrPattern(ExpressionOrPatternSyntax? node)
    {
        return Visit(node);
    }

    public virtual SyntaxTrivia VisitTrivia(SyntaxTrivia syntaxTrivia)
    {
        return syntaxTrivia;
    }
}
