namespace Raven.CodeAnalysis.Operations;

internal partial class OperationRewriter : OperationVisitor<Operation?>
{
    public override Operation? DefaultVisit(Operation operation)
    {
        foreach (var child in operation.ChildOperations)
            Visit(child);

        return operation;
    }
}
