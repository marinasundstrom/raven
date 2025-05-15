namespace Raven.CodeAnalysis;

class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }

    public BoundLiteralExpression(object value, ITypeSymbol type)
        : base(type, null, CandidateReason.None)
    {
        Value = value;
    }
}