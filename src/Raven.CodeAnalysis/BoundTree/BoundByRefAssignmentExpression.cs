using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundByRefAssignmentExpression : BoundAssignmentExpression
{
    public BoundByRefAssignmentExpression(BoundExpression reference, ITypeSymbol elementType, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, reference, right)
    {
        Reference = reference;
        ElementType = elementType;
    }

    public BoundExpression Reference { get; }

    public ITypeSymbol ElementType { get; }
}
