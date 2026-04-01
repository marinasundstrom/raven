using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundCatchClause : BoundNode
{
    public BoundCatchClause(ITypeSymbol exceptionType, ILocalSymbol? local, BoundPattern? pattern, BoundExpression? guard, BoundBlockStatement block)
    {
        ExceptionType = exceptionType;
        Local = local;
        Pattern = pattern;
        Guard = guard;
        Block = block;
    }

    public ITypeSymbol ExceptionType { get; }

    public ILocalSymbol? Local { get; }

    public BoundPattern? Pattern { get; }

    public BoundExpression? Guard { get; }

    public BoundBlockStatement Block { get; }
}
