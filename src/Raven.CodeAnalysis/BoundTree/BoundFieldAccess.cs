namespace Raven.CodeAnalysis;

internal partial class BoundFieldAccess : BoundExpression
{
    public BoundFieldAccess(BoundExpression? receiver, IFieldSymbol field, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(field.Type, field, reason)
    {
        Field = field;
        Receiver = receiver;
    }

    public BoundFieldAccess(IFieldSymbol field, BoundExpressionReason reason = BoundExpressionReason.None)
        : this(null, field, reason)
    {
    }

    public IFieldSymbol Field { get; }

    public BoundExpression? Receiver { get; }
}
