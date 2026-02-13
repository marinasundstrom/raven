using System.Collections.Immutable;
using System.Linq;
using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

abstract partial class BoundTreeRewriter : BoundTreeVisitor<BoundNode?>
{
    public override BoundNode? Visit(BoundNode? node)
    {
        if (node is null)
            return null;

        if (node is BoundPattern pattern)
            return VisitPattern(pattern);

        if (node is BoundDesignator designator)
            return VisitDesignator(designator);

        return node.Accept(this);
    }

    public virtual IEnumerable<T> VisitList<T>(IEnumerable<T> nodes)
        where T : BoundNode
    {
        foreach (var node in nodes)
            yield return (T)Visit(node)!;
    }

    public virtual ImmutableArray<T> VisitSymbolList<T>(IEnumerable<ISymbol?> symbols)
        where T : ISymbol
    {
        var builder = ImmutableArray.CreateBuilder<T>();

        if (symbols.TryGetNonEnumeratedCount(out var count))
            builder.Capacity = count;

        foreach (var symbol in symbols)
        {
            if (symbol is null)
            {
                builder.Add((T)symbol!);
                continue;
            }

            builder.Add((T)VisitSymbol(symbol)!);
        }

        return builder.MoveToImmutable();
    }

    public virtual BoundNode? VisitAssignmentExpression(BoundAssignmentExpression node) => node;

    public virtual BoundNode? VisitBreakStatement(BoundBreakStatement node) => node;

    public virtual BoundNode? VisitContinueStatement(BoundContinueStatement node) => node;

    public virtual INamespaceSymbol VisitNamespace(INamespaceSymbol @namespace)
    {
        return @namespace;
    }

    public virtual ISymbol? VisitSymbol(ISymbol? symbol)
    {
        return symbol switch
        {
            null => null,
            INamespaceSymbol ns => VisitNamespace(ns),
            ITypeSymbol type => VisitType(type),
            IMethodSymbol method => VisitMethod(method),
            IParameterSymbol parameter => VisitParameter(parameter),
            IPropertySymbol prop => VisitProperty(prop),
            IFieldSymbol field => VisitField(field),
            ILocalSymbol local => VisitLocal(local),
            ILabelSymbol label => VisitLabel(label),
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

    public virtual ILabelSymbol VisitLabel(ILabelSymbol label)
    {
        return label;
    }
}
