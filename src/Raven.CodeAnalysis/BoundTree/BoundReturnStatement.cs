using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundReturnStatement : BoundStatement
{
    public BoundReturnStatement(BoundExpression? expression)
    {
        Expression = expression;
    }

    public BoundExpression? Expression { get; }
}
