using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundAwaitExpression : BoundExpression
{
    public BoundAwaitExpression(
        BoundExpression expression,
        TypeSymbol resultType,
        TypeSymbol awaiterType,
        MethodSymbol getAwaiterMethod,
        MethodSymbol getResultMethod,
        PropertySymbol isCompletedProperty)
        : base(resultType, null, BoundExpressionReason.None)
    {
        Expression = expression;
        AwaiterType = awaiterType;
        GetAwaiterMethod = getAwaiterMethod;
        GetResultMethod = getResultMethod;
        IsCompletedProperty = isCompletedProperty;
    }

    public BoundExpression Expression { get; }

    public TypeSymbol AwaiterType { get; }

    public MethodSymbol GetAwaiterMethod { get; }

    public MethodSymbol GetResultMethod { get; }

    public PropertySymbol IsCompletedProperty { get; }

    public TypeSymbol ResultType => Type;
}
