using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundCatchClause : BoundNode
{
    public BoundCatchClause(ITypeSymbol exceptionType, ILocalSymbol? local, BoundBlockStatement block)
    {
        ExceptionType = exceptionType;
        Local = local;
        Block = block;
    }

    public ITypeSymbol ExceptionType { get; }

    public ILocalSymbol? Local { get; }

    public BoundBlockStatement Block { get; }
}
