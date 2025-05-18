namespace Raven.CodeAnalysis;

internal class BoundVoidExpression : BoundExpression
{
    public BoundVoidExpression(Compilation compilation, BoundExpressionReason candidateReason = BoundExpressionReason.None)
        : base(compilation.GetSpecialType(SpecialType.System_Void), null, candidateReason)
    {

    }
}
