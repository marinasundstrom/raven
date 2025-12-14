namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Represents a visitor that can traverse an operation tree.
/// </summary>
public abstract class OperationVisitor
{
    /// <summary>
    /// Visits an operation.
    /// </summary>
    public virtual void Visit(IOperation operation)
    {
        operation.Accept(this);
    }

    /// <summary>
    /// Performs a default visit.
    /// </summary>
    public virtual void DefaultVisit(IOperation operation)
    {
        foreach (var child in operation.ChildOperations)
            Visit(child);
    }
}

/// <summary>
/// Represents a visitor that can traverse an operation tree and return a value.
/// </summary>
public abstract class OperationVisitor<TResult>
{
    /// <summary>
    /// Visits an operation.
    /// </summary>
    public virtual TResult Visit(IOperation operation)
    {
        return operation.Accept(this);
    }

    /// <summary>
    /// Performs a default visit.
    /// </summary>
    public virtual TResult DefaultVisit(IOperation operation)
    {
        foreach (var child in operation.ChildOperations)
            Visit(child);

        return default!;
    }
}
