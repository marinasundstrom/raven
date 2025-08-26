namespace Raven.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;

internal partial class BoundForExpression : BoundExpression
{
    public ILocalSymbol Local { get; }
    public BoundExpression Collection { get; }
    public BoundExpression Body { get; }

    public BoundForExpression(ILocalSymbol local, BoundExpression collection, BoundExpression body)
        : base((collection.Type.GetElementType() ?? collection.Type).ContainingAssembly.GetTypeByMetadataName("Unit"), null, BoundExpressionReason.None)
    {
        Local = local;
        Collection = collection;
        Body = body;
    }
}
