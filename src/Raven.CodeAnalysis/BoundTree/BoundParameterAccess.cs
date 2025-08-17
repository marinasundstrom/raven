namespace Raven.CodeAnalysis;

internal partial class BoundParameterAccess : BoundExpression
{
    public BoundParameterAccess(IParameterSymbol parameter, BoundExpressionReason reason = BoundExpressionReason.None) : base(parameter.Type, parameter, reason)
    {
        Parameter = parameter;
    }

    public IParameterSymbol Parameter { get; }
}