namespace Raven.CodeAnalysis;

internal partial class BoundParameterAccess : BoundExpression
{
    public BoundParameterAccess(IParameterSymbol parameter, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(parameter.Type, parameter, candidateReason)
    {
        Parameter = parameter;
    }

    public IParameterSymbol Parameter { get; }
}