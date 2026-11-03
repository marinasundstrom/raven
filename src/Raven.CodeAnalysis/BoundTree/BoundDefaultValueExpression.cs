using System;

namespace Raven.CodeAnalysis;

internal partial class BoundDefaultValueExpression : BoundExpression
{
    public BoundDefaultValueExpression(TypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
    }
}
