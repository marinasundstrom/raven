namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Represents a visitor that can traverse an operation tree.
/// </summary>
public abstract partial class OperationVisitor
{
    /// <summary>
    /// Visits an operation.
    /// </summary>
    public virtual void Visit(Operation operation)
    {
        operation.Accept(this);
    }

    /// <summary>
    /// Performs a default visit.
    /// </summary>
    public virtual void DefaultVisit(Operation operation)
    {
        foreach (var child in operation.ChildOperations)
            Visit(child);
    }
}

/// <summary>
/// Represents a visitor that can traverse an operation tree and return a value.
/// </summary>
public abstract partial class OperationVisitor<TResult>
{
    /// <summary>
    /// Visits an operation.
    /// </summary>
    public virtual TResult Visit(Operation operation)
    {
        return operation.Accept(this);
    }

    /// <summary>
    /// Performs a default visit.
    /// </summary>
    public virtual TResult DefaultVisit(Operation operation)
    {
        foreach (var child in operation.ChildOperations)
            Visit(child);

        return default!;
    }
}
