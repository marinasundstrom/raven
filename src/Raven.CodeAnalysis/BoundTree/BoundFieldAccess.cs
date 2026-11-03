namespace Raven.CodeAnalysis;

internal partial class BoundFieldAccess : BoundExpression
{
    public BoundFieldAccess(BoundExpression? receiver, FieldSymbol field, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(field.Type, field, reason)
    {
        Field = field;
        Receiver = receiver;
    }

    public BoundFieldAccess(FieldSymbol field, BoundExpressionReason reason = BoundExpressionReason.None)
        : this(null, field, reason)
    {
    }

    public FieldSymbol Field { get; }

    public BoundExpression? Receiver { get; }
}
