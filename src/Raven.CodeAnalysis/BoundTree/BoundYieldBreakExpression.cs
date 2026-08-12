using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundYieldBreakExpression : BoundExpression
{
    public BoundYieldBreakExpression(ITypeSymbol elementType, IteratorMethodKind iteratorKind, ITypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        ElementType = elementType;
        IteratorKind = iteratorKind;
    }

    public ITypeSymbol ElementType { get; }

    public IteratorMethodKind IteratorKind { get; }
}
