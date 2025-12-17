namespace Raven.CodeAnalysis.Operations;

internal partial class OperationRewriter : OperationVisitor<IOperation?>
{
    public override IOperation? DefaultVisit(IOperation operation)
    {
        foreach (var child in operation.ChildOperations)
            Visit(child);

        return operation;
    }
}
