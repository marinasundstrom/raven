namespace Raven.CodeAnalysis;

internal partial class BoundFieldAccess : BoundExpression
{
    public BoundFieldAccess(IFieldSymbol field, BoundExpressionReason reason = BoundExpressionReason.None) : base(field.Type, field, reason)
    {
        Field = field;
    }

    public IFieldSymbol Field { get; }
}