using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

internal partial class BoundReturnStatement : BoundStatement
{
    public BoundReturnStatement(BoundExpression? expression)
    {
        Expression = expression;
    }

    public BoundExpression? Expression { get; }
}