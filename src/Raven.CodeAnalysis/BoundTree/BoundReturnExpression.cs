using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

internal partial class BoundReturnExpression : BoundExpression
{
    public BoundReturnExpression(BoundExpression? expression, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(expression.Type, null, candidateReason)
    {
        Expression = expression;
    }

    public BoundExpression? Expression { get; }
}