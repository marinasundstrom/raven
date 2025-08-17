namespace Raven.CodeAnalysis;

internal partial class BoundVoidExpression : BoundExpression
{
    public BoundVoidExpression(ITypeSymbol voidType, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(voidType, null, reason)
    {

    }

    public ITypeSymbol VoidType => Type;
}