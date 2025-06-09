namespace Raven.CodeAnalysis;

internal partial class BoundFieldAccess : BoundExpression
{
    public BoundFieldAccess(IFieldSymbol field, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(field.Type, field, candidateReason)
    {
        Field = field;
    }

    public IFieldSymbol Field { get; }
}