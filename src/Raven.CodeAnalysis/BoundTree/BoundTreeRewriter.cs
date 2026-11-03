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

    public virtual ImmutableArray<T> VisitSymbolList<T>(IEnumerable<Symbol?> symbols)
        where T : Symbol
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

    public override BoundNode? VisitBreakStatement(BoundBreakStatement node) => node;

    public override BoundNode? VisitContinueStatement(BoundContinueStatement node) => node;

    public override BoundNode? VisitCollectionComprehensionExpression(BoundCollectionComprehensionExpression node)
    {
        var type = VisitType(node.Type);
        var source = (BoundExpression)Visit(node.Source)!;
        var iterationLocal = VisitLocal(node.IterationLocal);
        var condition = (BoundExpression?)Visit(node.Condition);
        var selector = (BoundExpression)Visit(node.Selector)!;
        var elementType = VisitType(node.ElementType);

        if (ReferenceEquals(type, node.Type) &&
            ReferenceEquals(source, node.Source) &&
            ReferenceEquals(iterationLocal, node.IterationLocal) &&
            ReferenceEquals(condition, node.Condition) &&
            ReferenceEquals(selector, node.Selector) &&
            ReferenceEquals(elementType, node.ElementType))
        {
            return node;
        }

        return new BoundCollectionComprehensionExpression(
            type,
            source,
            iterationLocal,
            condition,
            selector,
            elementType,
            node.Reason);
    }

    public virtual NamespaceSymbol VisitNamespace(NamespaceSymbol @namespace)
    {
        return @namespace;
    }

    public virtual Symbol? VisitSymbol(Symbol? symbol)
    {
        return symbol switch
        {
            null => null,
            NamespaceSymbol ns => VisitNamespace(ns),
            TypeSymbol type => VisitType(type),
            MethodSymbol method => VisitMethod(method),
            ParameterSymbol parameter => VisitParameter(parameter),
            PropertySymbol prop => VisitProperty(prop),
            FieldSymbol field => VisitField(field),
            LocalSymbol local => VisitLocal(local),
            LabelSymbol label => VisitLabel(label),
            _ => throw new Exception($"Unhandled symbol type: {symbol.GetType()}")
        };
    }

    public virtual TypeSymbol VisitType(TypeSymbol type)
    {
        return type;
    }

    public virtual MethodSymbol VisitMethod(MethodSymbol method)
    {
        return method;
    }

    public virtual ParameterSymbol VisitParameter(ParameterSymbol param)
    {
        return param;
    }

    public virtual PropertySymbol VisitProperty(PropertySymbol property)
    {
        return property;
    }

    public virtual FieldSymbol VisitField(FieldSymbol field)
    {
        return field;
    }

    public virtual LocalSymbol VisitLocal(LocalSymbol local)
    {
        return local;
    }

    public virtual LabelSymbol VisitLabel(LabelSymbol label)
    {
        return label;
    }
}
