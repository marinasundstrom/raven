using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundYieldBreakStatement : BoundStatement
{
    public BoundYieldBreakStatement(ITypeSymbol elementType, IteratorMethodKind iteratorKind)
    {
        ElementType = elementType;
        IteratorKind = iteratorKind;
    }

    public ITypeSymbol ElementType { get; }

    public IteratorMethodKind IteratorKind { get; }
}
