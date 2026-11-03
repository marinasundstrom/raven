using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundCatchClause : BoundNode
{
    public BoundCatchClause(TypeSymbol exceptionType, LocalSymbol? local, BoundPattern? pattern, BoundExpression? guard, BoundBlockStatement block)
    {
        ExceptionType = exceptionType;
        Local = local;
        Pattern = pattern;
        Guard = guard;
        Block = block;
    }

    public TypeSymbol ExceptionType { get; }

    public LocalSymbol? Local { get; }

    public BoundPattern? Pattern { get; }

    public BoundExpression? Guard { get; }

    public BoundBlockStatement Block { get; }
}
