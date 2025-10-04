using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundAwaitExpression : BoundExpression
{
    public BoundAwaitExpression(
        BoundExpression expression,
        ITypeSymbol resultType,
        ITypeSymbol awaiterType,
        IMethodSymbol getAwaiterMethod,
        IMethodSymbol getResultMethod,
        IPropertySymbol isCompletedProperty)
        : base(resultType, null, BoundExpressionReason.None)
    {
        Expression = expression;
        AwaiterType = awaiterType;
        GetAwaiterMethod = getAwaiterMethod;
        GetResultMethod = getResultMethod;
        IsCompletedProperty = isCompletedProperty;
    }

    public BoundExpression Expression { get; }

    public ITypeSymbol AwaiterType { get; }

    public IMethodSymbol GetAwaiterMethod { get; }

    public IMethodSymbol GetResultMethod { get; }

    public IPropertySymbol IsCompletedProperty { get; }

    public ITypeSymbol ResultType => Type;
}
