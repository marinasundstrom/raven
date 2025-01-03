using System.Diagnostics;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class GreenNode
{
    internal IEnumerable<InternalSyntax.Diagnostic>? _diagnostics;

    public virtual SyntaxKind Kind { get; }
    public int Width { get; protected set; }
    public int FullWidth { get; protected set; }
    public int SlotCount { get; }

    internal InternalSyntax.SyntaxTriviaList LeadingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;
    internal InternalSyntax.SyntaxTriviaList TrailingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;

    internal GreenNode(SyntaxKind kind, int slotCount, IEnumerable<InternalSyntax.Diagnostic>? diagnostics = null)
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

    internal InternalSyntax.SyntaxToken? GetFirstTerminal()
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
                    var c = child.GetFirstTerminal();
                    if (c is not null)
                    {
                        return c;
                    }
                }
            }
        }

        return null;
    }

    internal InternalSyntax.SyntaxToken? GetLastTerminal()
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

    protected virtual GreenNode CreateParentWithNodes(IEnumerable<GreenNode> newNodes)
    {
        // This implementation depends on the specific type of the parent node.
        // For simplicity, you can return a SyntaxList for now, or customize it based on the context.

        return new SyntaxList(newNodes.ToArray());
    }

    protected abstract GreenNode WithUpdatedChildren(GreenNode[] newChildren);

    internal abstract IEnumerable<InternalSyntax.Diagnostic> GetDiagnostics();

    internal abstract GreenNode WithDiagnostics(params InternalSyntax.Diagnostic[] diagnostics);

    internal IEnumerable<InternalSyntax.Diagnostic> GetDiagnosticsNonRecursive()
    {
        return _diagnostics ?? Enumerable.Empty<InternalSyntax.Diagnostic>();
    }

    private string GetDebuggerDisplay() => $"{Kind} {GetValueText()}";
}
