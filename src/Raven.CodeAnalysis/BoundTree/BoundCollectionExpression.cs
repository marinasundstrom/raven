namespace Raven.CodeAnalysis;

internal partial class BoundCollectionExpression : BoundExpression
{
    public BoundCollectionExpression(
        ITypeSymbol type,
        IEnumerable<BoundExpression> elements,
        ISymbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
        Elements = elements;
        CollectionSymbol = collectionSymbol;
    }

    public IEnumerable<BoundExpression> Elements { get; }

    public ISymbol? CollectionSymbol { get; }
}

internal sealed class BoundDictionaryEntry
{
    public BoundDictionaryEntry(BoundExpression key, BoundExpression value)
    {
        Key = key;
        Value = value;
    }

    public BoundExpression Key { get; }

    public BoundExpression Value { get; }
}

internal partial class BoundDictionaryExpression : BoundExpression
{
    public BoundDictionaryExpression(
        ITypeSymbol type,
        IEnumerable<BoundDictionaryEntry> entries,
        ISymbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
        Entries = entries;
        CollectionSymbol = collectionSymbol;
    }

    public IEnumerable<BoundDictionaryEntry> Entries { get; }

    public ISymbol? CollectionSymbol { get; }
}

internal partial class BoundEmptyCollectionExpression : BoundExpression
{
    public BoundEmptyCollectionExpression(
        ITypeSymbol? type = null,
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
        ITypeSymbol type,
        BoundExpression source,
        ILocalSymbol iterationLocal,
        BoundExpression? condition,
        BoundExpression selector,
        ITypeSymbol elementType,
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
    public ILocalSymbol IterationLocal { get; }
    public BoundExpression? Condition { get; }
    public BoundExpression Selector { get; }
    public ITypeSymbol ElementType { get; }

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.VisitCollectionComprehensionExpression(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.VisitCollectionComprehensionExpression(this);
    }
}
