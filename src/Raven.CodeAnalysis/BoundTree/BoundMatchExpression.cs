using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundMatchExpression : BoundExpression
{
    public BoundMatchExpression(
        BoundExpression expression,
        ImmutableArray<BoundMatchArm> arms,
        ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Expression = expression;
        Arms = arms;
    }

    public BoundExpression Expression { get; }

    public ImmutableArray<BoundMatchArm> Arms { get; }
}

internal sealed class BoundMatchArm
{
    public BoundMatchArm(BoundPattern pattern, BoundExpression? guard, BoundExpression expression)
    {
        Pattern = pattern;
        Guard = guard;
        Expression = expression;
    }

    public BoundPattern Pattern { get; }

    public BoundExpression? Guard { get; }

    public BoundExpression Expression { get; }
}

