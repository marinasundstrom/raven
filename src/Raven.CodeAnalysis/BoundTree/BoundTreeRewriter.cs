using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

abstract partial class BoundTreeRewriter : BoundTreeVisitor<BoundNode?>
{
    public override BoundNode? Visit(BoundNode? node)
    {
        return node?.Accept(this);
    }

    public virtual IEnumerable<T> VisitList<T>(IEnumerable<T> nodes)
        where T : BoundNode
    {
        foreach (var node in nodes)
            yield return (T)node.Accept(this)!;
    }

    public virtual BoundExpression VisitExpression(BoundExpression node)
    {
        switch (node)
        {
            case BoundLiteralExpression lit:
                VisitLiteralExpression(lit);
                break;
            case BoundLocalAccess local:
                VisitLocalAccess(local);
                break;
            case BoundParameterAccess par:
                VisitParameterAccess(par);
                break;
            case BoundBinaryExpression bin:
                VisitBinaryExpression(bin);
                break;
            case BoundInvocationExpression call:
                VisitInvocationExpression(call);
                break;
            case BoundLambdaExpression lambda:
                VisitLambdaExpression(lambda);
                break;
            case BoundBlockExpression block:
                VisitBlockExpression(block);
                break;
        }

        throw new NotImplementedException($"Unhandled expression: {node.GetType().Name}");
    }

    public virtual INamedTypeSymbol VisitNamespace(INamedTypeSymbol @namespace)
    {
        return @namespace;
    }

    public virtual ISymbol VisitSymbol(ISymbol symbol)
    {
        return symbol;
    }

    public virtual ITypeSymbol VisitType(ITypeSymbol type)
    {
        return type;
    }

    public virtual IMethodSymbol VisitMethod(IMethodSymbol method)
    {
        return method;
    }

    public virtual IPropertySymbol VisitProperty(IPropertySymbol property)
    {
        return property;
    }

    public virtual IFieldSymbol VisitField(IFieldSymbol field)
    {
        return field;
    }

    public virtual ILocalSymbol VisitLocal(ILocalSymbol local)
    {
        return local;
    }

    public virtual BoundPattern VisitPattern(BoundPattern pattern)
    {
        return pattern;
    }

    public virtual BoundDesignator VisitDesignator(BoundDesignator designator)
    {
        return designator;
    }
}