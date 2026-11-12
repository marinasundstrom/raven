using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Base type for concrete operations.
/// </summary>
public abstract class OperationBase : Operation
{
    private readonly Lazy<ImmutableArray<Operation>> _lazyChildren;
    private OperationBase? _parent;

    protected OperationBase(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        TypeSymbol? type,
        bool isImplicit)
    {
        SemanticModel = semanticModel;
        Kind = kind;
        Syntax = syntax;
        Type = type;
        IsImplicit = isImplicit;
        _lazyChildren = new Lazy<ImmutableArray<Operation>>(ComputeChildren);
    }

    /// <inheritdoc />
    public SemanticModel SemanticModel { get; }

    /// <inheritdoc />
    public OperationKind Kind { get; }

    /// <inheritdoc />
    public SyntaxNode Syntax { get; }

    /// <inheritdoc />
    public TypeSymbol? Type { get; }

    /// <inheritdoc />
    public bool IsImplicit { get; }

    /// <inheritdoc />
    public ImmutableArray<Operation> ChildOperations => _lazyChildren.Value;

    /// <inheritdoc />
    public Operation? Parent => _parent;

    /// <inheritdoc />
    public virtual void Accept(OperationVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    /// <inheritdoc />
    public virtual TResult Accept<TResult>(OperationVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }

    /// <summary>
    /// Computes the set of child operations for this node.
    /// </summary>
    protected abstract ImmutableArray<Operation> GetChildrenCore();

    private ImmutableArray<Operation> ComputeChildren()
    {
        var children = GetChildrenCore();

        foreach (var child in children)
        {
            if (child is OperationBase op)
                op.SetParent(this);
        }

        return children;
    }

    internal void SetParent(OperationBase? parent)
    {
        _parent = parent;
    }
}
