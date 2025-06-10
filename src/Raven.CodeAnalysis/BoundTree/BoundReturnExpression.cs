using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

internal partial class BoundReturnStatement : BoundStatement
{
    public BoundReturnStatement(BoundExpression? expression, BoundExpressionReason candidateReason = BoundExpressionReason.None)
    {
        Expression = expression;
    }

    public BoundExpression? Expression { get; }
}