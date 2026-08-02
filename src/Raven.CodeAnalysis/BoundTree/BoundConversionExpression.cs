namespace Raven.CodeAnalysis;

internal partial class BoundConversionExpression : BoundExpression
{
    public BoundExpression Expression { get; }
    public Conversion Conversion { get; }
    public bool IsNullabilityFlowNarrowing { get; }
    public bool IsNullableSuppression { get; }

    public BoundConversionExpression(
        BoundExpression expression,
        ITypeSymbol type,
        Conversion conversion,
        bool isNullabilityFlowNarrowing = false,
        bool isNullableSuppression = false)
        : base(type)
    {
        Expression = expression;
        Conversion = conversion;
        IsNullabilityFlowNarrowing = isNullabilityFlowNarrowing;
        IsNullableSuppression = isNullableSuppression;
    }

    public bool IsExplicit => Conversion.IsExplicit;

    public bool IsBoxing => Conversion.IsBoxing;

    public bool IsReference => Conversion.IsReference;

    public bool IsIdentity => Conversion.IsIdentity;

    public bool IsUserDefined => Conversion.IsUserDefined;

    public IMethodSymbol? MethodSymbol => Conversion.MethodSymbol;

}
