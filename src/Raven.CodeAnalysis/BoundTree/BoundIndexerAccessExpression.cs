namespace Raven.CodeAnalysis;

internal partial class BoundIndexerAccessExpression : BoundExpression
{
    public BoundExpression Receiver { get; }
    public IEnumerable<BoundExpression> Arguments { get; }
    public IPropertySymbol Indexer { get; }

    public BoundIndexerAccessExpression(BoundExpression receiver, IEnumerable<BoundExpression> arguments, IPropertySymbol indexer)
        : base(indexer.Type, indexer, BoundExpressionReason.None)
    {
        Receiver = receiver;
        Arguments = arguments;
        Indexer = indexer;
    }
}