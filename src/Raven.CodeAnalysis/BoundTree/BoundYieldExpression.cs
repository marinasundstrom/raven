using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundYieldExpression : BoundExpression
{
    public BoundYieldExpression(
        BoundExpression expression,
        TypeSymbol elementType,
        IteratorMethodKind iteratorKind,
        TypeSymbol type, ForIterationInfo? iteration = null)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        Expression = expression;
        ElementType = elementType;
        IteratorKind = iteratorKind;
        Iteration = iteration;
    }

    public BoundExpression Expression { get; }

    public ForIterationInfo? Iteration { get; }

    public TypeSymbol ElementType { get; }

    public IteratorMethodKind IteratorKind { get; }
}
