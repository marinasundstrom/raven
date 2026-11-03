namespace Raven.CodeAnalysis;

internal partial class BoundCollectionExpression : BoundExpression
{
    public BoundCollectionExpression(
        TypeSymbol type,
        IEnumerable<BoundExpression> elements,
        Symbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
        Elements = elements;
        CollectionSymbol = collectionSymbol;
    }

    public IEnumerable<BoundExpression> Elements { get; }

    public Symbol? CollectionSymbol { get; }
}

internal abstract class DictionaryElementBinding;

internal sealed class DictionaryEntryBinding : DictionaryElementBinding
{
    public DictionaryEntryBinding(BoundExpression key, BoundExpression value)
    {
        Key = key;
        Value = value;
    }

    public BoundExpression Key { get; }

    public BoundExpression Value { get; }
}

internal sealed class DictionarySpreadBinding : DictionaryElementBinding
{
    public DictionarySpreadBinding(BoundExpression expression)
    {
        Expression = expression;
    }

    public BoundExpression Expression { get; }
}

internal sealed class DictionaryComprehensionBinding : DictionaryElementBinding
{
    public DictionaryComprehensionBinding(
        BoundExpression source,
        LocalSymbol iterationLocal,
        BoundExpression? condition,
        BoundExpression keySelector,
        BoundExpression valueSelector,
        TypeSymbol keyType,
        TypeSymbol valueType)
    {
        Source = source;
        IterationLocal = iterationLocal;
        Condition = condition;
        KeySelector = keySelector;
        ValueSelector = valueSelector;
        KeyType = keyType;
        ValueType = valueType;
    }

    public BoundExpression Source { get; }
    public LocalSymbol IterationLocal { get; }
    public BoundExpression? Condition { get; }
    public BoundExpression KeySelector { get; }
    public BoundExpression ValueSelector { get; }
    public TypeSymbol KeyType { get; }
    public TypeSymbol ValueType { get; }
}

internal partial class BoundDictionaryExpression : BoundExpression
{
    public BoundDictionaryExpression(
        TypeSymbol type,
        IEnumerable<DictionaryElementBinding> elements,
        Symbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
        Elements = elements;
        CollectionSymbol = collectionSymbol;
    }

    public IEnumerable<DictionaryElementBinding> Elements { get; }

    public Symbol? CollectionSymbol { get; }
}

internal partial class BoundEmptyCollectionExpression : BoundExpression
{
    public BoundEmptyCollectionExpression(
        TypeSymbol? type = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type!, null, reason)
    {

    }
}

internal partial class BoundSpreadElement : BoundExpression
{
    public BoundSpreadElement(BoundExpression expression)
        : base(expression.Type!, expression.Symbol, expression.Reason)
    {
        Expression = expression;
    }

    public BoundExpression Expression { get; }
}

internal sealed class BoundCollectionComprehensionExpression : BoundExpression
{
    public BoundCollectionComprehensionExpression(
        TypeSymbol type,
        BoundExpression source,
        LocalSymbol iterationLocal,
        BoundExpression? condition,
        BoundExpression selector,
        TypeSymbol elementType,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
        Source = source;
        IterationLocal = iterationLocal;
        Condition = condition;
        Selector = selector;
        ElementType = elementType;
    }

    public BoundExpression Source { get; }
    public LocalSymbol IterationLocal { get; }
    public BoundExpression? Condition { get; }
    public BoundExpression Selector { get; }
    public TypeSymbol ElementType { get; }

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.VisitCollectionComprehensionExpression(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.VisitCollectionComprehensionExpression(this);
    }
}
