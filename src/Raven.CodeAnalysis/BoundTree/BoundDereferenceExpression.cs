using System;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundDereferenceExpression : BoundExpression
{
    public BoundDereferenceExpression(BoundExpression reference, ITypeSymbol elementType)
        : base(elementType ?? throw new ArgumentNullException(nameof(elementType)), reference?.Symbol)
    {
        Reference = reference ?? throw new ArgumentNullException(nameof(reference));
        ElementType = elementType;
    }

    public BoundExpression Reference { get; }

    public ITypeSymbol ElementType { get; }
}
