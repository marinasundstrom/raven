using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Base type for concrete operations.
/// </summary>
public abstract class Operation : IOperation
{
    private readonly Lazy<ImmutableArray<IOperation>> _lazyChildren;
    private Operation? _parent;

    protected Operation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        ITypeSymbol? type,
        bool isImplicit)
    {
        SemanticModel = semanticModel;
        Kind = kind;
        Syntax = syntax;
        Type = type;
        IsImplicit = isImplicit;
        _lazyChildren = new Lazy<ImmutableArray<IOperation>>(ComputeChildren);
    }

    /// <inheritdoc />
    public SemanticModel SemanticModel { get; }

    /// <inheritdoc />
    public OperationKind Kind { get; }

    /// <inheritdoc />
    public SyntaxNode Syntax { get; }

    /// <inheritdoc />
    public ITypeSymbol? Type { get; }

    /// <inheritdoc />
    public bool IsImplicit { get; }

    /// <inheritdoc />
    public ImmutableArray<IOperation> Children => _lazyChildren.Value;

    /// <inheritdoc />
    public IOperation? Parent => _parent;

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
    protected abstract ImmutableArray<IOperation> GetChildrenCore();

    private ImmutableArray<IOperation> ComputeChildren()
    {
        var children = GetChildrenCore();

        foreach (var child in children)
        {
            if (child is Operation op)
                op.SetParent(this);
        }

        return children;
    }

    internal void SetParent(Operation? parent)
    {
        _parent = parent;
    }
}
