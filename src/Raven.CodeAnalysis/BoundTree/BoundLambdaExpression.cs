namespace Raven.CodeAnalysis;

internal partial class BoundLambdaExpression : BoundExpression
{
    public IReadOnlyList<IParameterSymbol> Parameters { get; }
    public BoundExpression Body { get; }
    public ITypeSymbol ReturnType { get; }
    public ITypeSymbol DelegateType { get; }
    public IReadOnlyList<ISymbol> CapturedVariables { get; }

    public BoundLambdaExpression(
        IReadOnlyList<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        BoundExpression body,
        ISymbol symbol,
        ITypeSymbol delegateType,
        IReadOnlyList<ISymbol> capturedVariables)
        : base(delegateType, symbol, BoundExpressionReason.None)
    {
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
        DelegateType = delegateType;
        CapturedVariables = capturedVariables;
    }
}