using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundYieldExpression : BoundExpression
{
    public BoundYieldExpression(
        BoundExpression expression,
        ITypeSymbol elementType,
        IteratorMethodKind iteratorKind,
        ITypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        Expression = expression;
        ElementType = elementType;
        IteratorKind = iteratorKind;
    }

    public BoundExpression Expression { get; }

    public ITypeSymbol ElementType { get; }

    public IteratorMethodKind IteratorKind { get; }
}
