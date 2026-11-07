namespace Raven.CodeAnalysis;

internal sealed partial class BoundParameterAccess : BoundExpression
{
    public BoundParameterAccess(ParameterSymbol parameter, BoundExpressionReason reason = BoundExpressionReason.None) : base(parameter.Type, parameter, reason)
    {
        Parameter = parameter;
    }

    public ParameterSymbol Parameter { get; }
}
