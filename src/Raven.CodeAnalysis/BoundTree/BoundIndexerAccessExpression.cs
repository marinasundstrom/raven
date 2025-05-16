namespace Raven.CodeAnalysis;

sealed class BoundIndexerAccessExpression : BoundExpression
{
    public BoundExpression Receiver { get; }
    public BoundExpression[] Arguments { get; }
    public IPropertySymbol Indexer { get; }

    public BoundIndexerAccessExpression(BoundExpression receiver, BoundExpression[] arguments, IPropertySymbol indexer)
        : base(indexer.Type, indexer, BoundExpressionReason.None)
    {
        Receiver = receiver;
        Arguments = arguments;
        Indexer = indexer;
    }
}