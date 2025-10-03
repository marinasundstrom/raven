using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundForStatement : BoundStatement
{
    public ILocalSymbol Local { get; }
    public ForIterationInfo Iteration { get; }
    public BoundExpression Collection { get; }
    public BoundStatement Body { get; }

    public BoundForStatement(
        ILocalSymbol local,
        ForIterationInfo iteration,
        BoundExpression collection,
        BoundStatement body)
    {
        Local = local;
        Iteration = iteration;
        Collection = collection;
        Body = body;
    }
}
