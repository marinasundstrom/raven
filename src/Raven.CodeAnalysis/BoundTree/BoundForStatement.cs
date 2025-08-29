using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundForStatement : BoundStatement
{
    public ILocalSymbol Local { get; }
    public BoundExpression Collection { get; }
    public BoundStatement Body { get; }

    public BoundForStatement(ILocalSymbol local, BoundExpression collection, BoundStatement body)
    {
        Local = local;
        Collection = collection;
        Body = body;
    }
}
