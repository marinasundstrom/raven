using System;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundDefaultValueExpression : BoundExpression
{
    public BoundDefaultValueExpression(TypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
    }
}
