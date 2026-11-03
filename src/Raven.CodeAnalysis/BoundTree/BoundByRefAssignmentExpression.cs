using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundByRefAssignmentExpression : BoundAssignmentExpression
{
    public BoundByRefAssignmentExpression(BoundExpression reference, TypeSymbol elementType, BoundExpression right, TypeSymbol unitType)
        : base(unitType, reference, right)
    {
        Reference = reference;
        ElementType = elementType;
    }

    public BoundExpression Reference { get; }

    public TypeSymbol ElementType { get; }
}
