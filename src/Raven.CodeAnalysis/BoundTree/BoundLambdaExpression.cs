namespace Raven.CodeAnalysis;

internal partial class BoundLambdaExpression : BoundExpression
{
    public IEnumerable<IParameterSymbol> Parameters { get; }
    public BoundExpression Body { get; }
    public ITypeSymbol ReturnType { get; }
    public ITypeSymbol DelegateType { get; }
    public IEnumerable<ISymbol> CapturedVariables { get; }

    public BoundLambdaExpression(
        IEnumerable<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        BoundExpression body,
        ISymbol symbol,
        ITypeSymbol delegateType,
        IEnumerable<ISymbol> capturedVariables)
        : base(delegateType, symbol, BoundExpressionReason.None)
    {
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
        DelegateType = delegateType;
        CapturedVariables = capturedVariables;
    }
}