using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundYieldStatement : BoundStatement
{
    public BoundYieldStatement(BoundExpression expression, ITypeSymbol elementType, IteratorMethodKind iteratorKind)
    {
        Expression = expression ?? throw new ArgumentNullException(nameof(expression));
        ElementType = elementType;
        IteratorKind = iteratorKind;
    }

    public BoundExpression Expression { get; }

    public ITypeSymbol ElementType { get; }

    public IteratorMethodKind IteratorKind { get; }
}
