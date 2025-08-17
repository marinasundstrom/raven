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

    public virtual IEnumerable<T> VisitSymbolList<T>(IEnumerable<ISymbol> symbols)
        where T : ISymbol
    {
        foreach (var symbol in symbols)
            yield return (T)VisitSymbol(symbol)!;
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

    public virtual INamespaceSymbol VisitNamespace(INamespaceSymbol @namespace)
    {
        return @namespace;
    }

    public virtual ISymbol VisitSymbol(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol ns => VisitNamespace(ns),
            ITypeSymbol type => VisitType(type),
            IMethodSymbol method => VisitMethod(method),
            IParameterSymbol parameter => VisitParameter(parameter),
            IPropertySymbol prop => VisitProperty(prop),
            IFieldSymbol field => VisitField(field),
            ILocalSymbol local => VisitLocal(local),
            _ => throw new Exception($"Unhandled symbol type: {symbol.GetType()}")
        };
    }

    public virtual ITypeSymbol VisitType(ITypeSymbol type)
    {
        return type;
    }

    public virtual IMethodSymbol VisitMethod(IMethodSymbol method)
    {
        return method;
    }

    public virtual IParameterSymbol VisitParameter(IParameterSymbol param)
    {
        return param;
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

    public virtual BoundNode VisitPattern(BoundPattern pattern)
    {
        return pattern.Accept(this);
    }

    public virtual BoundNode VisitDesignator(BoundDesignator designator)
    {
        return designator.Accept(this);
    }
}