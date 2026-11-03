namespace Raven.CodeAnalysis;

// The non-null value supplied by the enclosing conditional-access branch.
// It carries no receiver expression, so rewrites cannot duplicate its evaluation.
internal partial class BoundConditionalReceiverExpression : BoundExpression
{
    public BoundConditionalReceiverExpression(TypeSymbol type) : base(type)
    {
    }
}
