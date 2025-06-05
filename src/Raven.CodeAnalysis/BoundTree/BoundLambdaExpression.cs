namespace Raven.CodeAnalysis;

internal sealed class BoundLambdaExpression : BoundExpression
{
    public IReadOnlyList<IParameterSymbol> Parameters { get; }
    public BoundExpression Body { get; }
    public ITypeSymbol ReturnType { get; }
    public ITypeSymbol DelegateType { get; }

    public BoundLambdaExpression(
        IReadOnlyList<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        BoundExpression body,
        IMethodSymbol symbol,
        ITypeSymbol delegateType)
        : base(delegateType, symbol, BoundExpressionReason.None)
    {
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
        DelegateType = delegateType;
    }
}