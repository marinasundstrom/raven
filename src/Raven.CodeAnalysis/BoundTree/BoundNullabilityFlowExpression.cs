namespace Raven.CodeAnalysis;

/// <summary>
/// Preserves an expression's declared type while publishing a distinct
/// nullability flow type. The wrapper is transparent to bound-tree visitors.
/// </summary>
internal sealed class BoundNullabilityFlowExpression : BoundExpression
{
    public BoundNullabilityFlowExpression(BoundExpression expression, ITypeSymbol flowType)
        : base(expression.Type, expression.Symbol, expression.Reason)
    {
        Expression = expression;
        FlowType = flowType;
    }

    public BoundExpression Expression { get; }

    public ITypeSymbol FlowType { get; }

    public override ITypeSymbol? GetConvertedType() => Expression.GetConvertedType();

    public override ITypeSymbol? GetNaturalType() => Expression.GetNaturalType();

    public override void Accept(BoundTreeVisitor visitor) => Expression.Accept(visitor);

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => Expression.Accept(visitor);
}
