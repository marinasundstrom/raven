using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal partial class BoundIndexerAccessExpression : BoundExpression
{
    public BoundExpression Receiver { get; }
    public IEnumerable<BoundExpression> Arguments { get; }
    public PropertySymbol Indexer { get; }
    public IEnumerable<BoundStatement> Captures { get; }

    public BoundIndexerAccessExpression(BoundExpression receiver, IEnumerable<BoundExpression> arguments, PropertySymbol indexer, IEnumerable<BoundStatement>? captures = null)
        : base(indexer.Type, indexer, BoundExpressionReason.None)
    {
        Receiver = receiver;
        Arguments = arguments;
        Indexer = indexer;
        Captures = captures?.ToImmutableArray() ?? [];
    }
}
