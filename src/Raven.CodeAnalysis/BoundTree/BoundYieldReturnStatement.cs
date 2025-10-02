using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundYieldReturnStatement : BoundStatement
{
    public BoundYieldReturnStatement(BoundExpression expression, ITypeSymbol elementType, IteratorMethodKind iteratorKind)
    {
        Expression = expression ?? throw new ArgumentNullException(nameof(expression));
        ElementType = elementType;
        IteratorKind = iteratorKind;
    }

    public BoundExpression Expression { get; }

    public ITypeSymbol ElementType { get; }

    public IteratorMethodKind IteratorKind { get; }
}
