using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundMatchStatement : BoundStatement
{
    public BoundMatchStatement(BoundExpression expression, ImmutableArray<BoundMatchArm> arms)
    {
        Expression = expression;
        Arms = arms;
    }

    public BoundExpression Expression { get; }

    public ImmutableArray<BoundMatchArm> Arms { get; }

    public override Symbol? Symbol => Expression.Symbol;
}
