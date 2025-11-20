using System;

namespace Raven.CodeAnalysis;

internal partial class BoundDefaultValueExpression : BoundExpression
{
    public BoundDefaultValueExpression(ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
    }
}
