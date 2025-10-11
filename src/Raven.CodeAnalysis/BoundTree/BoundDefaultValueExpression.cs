using System;

namespace Raven.CodeAnalysis;

internal sealed class BoundDefaultValueExpression : BoundExpression
{
    public BoundDefaultValueExpression(ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
    }

    public override void Accept(BoundTreeVisitor visitor)
    {
        if (visitor is null)
            throw new ArgumentNullException(nameof(visitor));

        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        if (visitor is null)
            throw new ArgumentNullException(nameof(visitor));

        return visitor.DefaultVisit(this);
    }
}
