using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal partial class BoundTryStatement : BoundStatement
{
    public BoundTryStatement(
        BoundBlockStatement tryBlock,
        ImmutableArray<BoundCatchClause> catchClauses,
        BoundBlockStatement? finallyBlock,
        BoundTryStatementKind kind = BoundTryStatementKind.UserAuthored)
    {
        TryBlock = tryBlock;
        CatchClauses = catchClauses;
        FinallyBlock = finallyBlock;
        Kind = kind;
    }

    public BoundBlockStatement TryBlock { get; }

    public ImmutableArray<BoundCatchClause> CatchClauses { get; }

    public BoundBlockStatement? FinallyBlock { get; }

    public BoundTryStatementKind Kind { get; }

    public BoundTryStatement Update(
        BoundBlockStatement tryBlock,
        IEnumerable<BoundCatchClause> catchClauses,
        BoundBlockStatement? finallyBlock)
    {
        return Update(tryBlock, ImmutableArray.CreateRange(catchClauses), finallyBlock, Kind);
    }

    public BoundTryStatement Update(
        BoundBlockStatement tryBlock,
        IEnumerable<BoundCatchClause> catchClauses,
        BoundBlockStatement? finallyBlock,
        BoundTryStatementKind kind)
    {
        return new BoundTryStatement(tryBlock, ImmutableArray.CreateRange(catchClauses), finallyBlock, kind);
    }

}
