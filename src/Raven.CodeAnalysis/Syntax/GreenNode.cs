using System.Diagnostics;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class GreenNode
{
    internal IEnumerable<DiagnosticInfo>? _diagnostics;

    public virtual SyntaxKind Kind { get; }

    public bool IsKind(SyntaxKind kind) => Kind == kind;

    public int Width { get; protected set; }
    public int FullWidth { get; protected set; }
    public int SlotCount { get; }

    internal InternalSyntax.SyntaxTriviaList LeadingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;
    internal InternalSyntax.SyntaxTriviaList TrailingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;

    internal GreenNode(SyntaxKind kind, int slotCount, IEnumerable<DiagnosticInfo>? diagnostics = null)
    {
        Kind = kind;
        SlotCount = slotCount;
        _diagnostics = diagnostics;
    }

    public virtual bool IsMissing { get; }

    public abstract GreenNode GetSlot(int index);

    internal IEnumerable<GreenNode> GetChildren()
    {
        for (int i = 0; i < SlotCount; i++)
        {
            var child = GetSlot(i);

            if (child is null)
                continue;

            if (child is InternalSyntax.SyntaxList)
            {
                for (int i2 = 0; i2 < child.SlotCount; i2++)
                {
                    var child2 = child.GetSlot(i2);

                    if (child2 is null)
                        continue;

                    yield return child2;
                }
            }
            else
            {
                yield return child;
            }
        }
    }

    internal InternalSyntax.SyntaxToken? GetFirstToken()
    {
        GreenNode? node = this;

        for (int i = 0, n = node.SlotCount; i < n; i++)
        {
            var child = node.GetSlot(i);
            if (child != null)
            {
                if (child is InternalSyntax.SyntaxToken t)
                {
                    return t;
                }
                else
                {
                    var c = child.GetFirstToken();
                    if (c is not null)
                    {
                        return c;
                    }
                }
            }
        }

        return null;
    }

    internal InternalSyntax.SyntaxToken? GetLastToken()
    {
        GreenNode? node = this;

        do
        {
            GreenNode? lastChild = null;

            for (int i = node.SlotCount - 1; i >= 0; i--)
            {
                var child = node.GetSlot(i);
                if (child != null)
                {
                    lastChild = child;
                    break;
                }
            }

            node = lastChild;
        }
        while (node != null && node is not InternalSyntax.SyntaxToken);

        return (InternalSyntax.SyntaxToken?)node;
    }

    public virtual SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return null!;
    }

    public virtual object? GetValue() => (int)Kind;

    public virtual string? GetValueText() => SyntaxFacts.GetSyntaxTokenText(Kind);

    public virtual int GetChildStartPosition(int childIndex)
    {
        int offset = 0;

        for (int i = 0; i < childIndex; i++)
        {
            var slot = GetSlot(i);
            if (slot != null)
            {
                offset += slot.FullWidth;
            }
        }

        return offset;
    }
    public GreenNode ReplaceNode(GreenNode oldNode, GreenNode newNode)
    {
        if (this == oldNode)
        {
            return newNode;
        }

        var updatedChildren = new GreenNode[this.SlotCount];
        for (int i = 0; i < this.SlotCount; i++)
        {
            var child = this.GetSlot(i);
            if (child == oldNode)
            {
                updatedChildren[i] = newNode;
            }
            else
            {
                updatedChildren[i] = child?.ReplaceNode(oldNode, newNode) ?? child;
            }
        }

        return WithUpdatedChildren(updatedChildren);
    }

    public GreenNode ReplaceNode(GreenNode oldNode, IEnumerable<GreenNode> newNodes)
    {
        return ReplaceNodeWithNodes(oldNode, newNodes);
    }

    private GreenNode ReplaceNodeWithNodes(GreenNode oldNode, IEnumerable<GreenNode> newNodes)
    {
        if (this == oldNode)
        {
            // If the current node is the one to replace, return a new parent with the new nodes
            return CreateParentWithNodes(newNodes);
        }

        // Traverse child nodes to find the node to replace
        var updatedChildren = new List<GreenNode>();
        for (int i = 0; i < SlotCount; i++)
        {
            var child = GetSlot(i);

            if (child == oldNode)
            {
                // Add the new nodes in place of the old node
                updatedChildren.AddRange(newNodes);
            }
            else if (child != null)
            {
                // Recur for other children
                updatedChildren.Add(child.ReplaceNodeWithNodes(oldNode, newNodes));
            }
        }

        // Create a new green node with updated children
        return WithUpdatedChildren(updatedChildren.ToArray());
    }

    public GreenNode ReplaceNodes(Func<GreenNode, bool> condition, Func<GreenNode, GreenNode> replacement)
    {
        // If the current node matches the condition, replace it using the replacement function
        if (condition(this))
        {
            return replacement(this);
        }

        // Otherwise, traverse and replace children recursively
        var updatedChildren = new GreenNode[this.SlotCount];
        bool anyChildReplaced = false;

        for (int i = 0; i < this.SlotCount; i++)
        {
            var child = this.GetSlot(i);

            if (child != null)
            {
                var updatedChild = child.ReplaceNodes(condition, replacement);

                if (!ReferenceEquals(updatedChild, child))
                {
                    anyChildReplaced = true;
                }

                updatedChildren[i] = updatedChild;
            }
            else
            {
                updatedChildren[i] = child;
            }
        }

        // If no children were replaced, return the current node to avoid creating unnecessary copies
        if (!anyChildReplaced)
        {
            return this;
        }

        // Otherwise, return a new node with the updated children
        return WithUpdatedChildren(updatedChildren);
    }

    protected virtual GreenNode CreateParentWithNodes(IEnumerable<GreenNode> newNodes)
    {
        // This implementation depends on the specific type of the parent node.
        // For simplicity, you can return a SyntaxList for now, or customize it based on the context.

        return new SyntaxList(newNodes.ToArray());
    }

    protected abstract GreenNode WithUpdatedChildren(GreenNode[] newChildren);

    internal IEnumerable<DiagnosticInfo> GetDiagnostics()
    {
        return _diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    internal abstract IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive();

    internal abstract GreenNode SetDiagnostics(params DiagnosticInfo[] diagnostics);

    private string GetDebuggerDisplay() => $"{Kind} {GetValueText()}";

    internal abstract void Accept(InternalSyntax.SyntaxVisitor visitor);

    internal abstract TResult Accept<TResult>(InternalSyntax.SyntaxVisitor<TResult> visitor);
}