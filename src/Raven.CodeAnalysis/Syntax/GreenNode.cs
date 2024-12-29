namespace Raven.CodeAnalysis.Syntax;

public abstract class GreenNode
{
    public virtual SyntaxKind Kind { get; }
    public int Width { get; }
    public int FullWidth { get; }
    public int SlotCount { get; }

    internal InternalSyntax.SyntaxTriviaList LeadingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;
    internal InternalSyntax.SyntaxTriviaList TrailingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;

    protected GreenNode(SyntaxKind kind, int slotCount, int width, int fullWidth)
    {
        Kind = kind;
        SlotCount = slotCount;
        Width = width;
        FullWidth = fullWidth;
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

    internal SyntaxToken? GetFirstTerminal()
    {
        GreenNode? node = this;

        for (int i = 0, n = node.SlotCount; i < n; i++)
        {
            var child = node.GetSlot(i);
            if (child != null)
            {
                if (child is InternalSyntax.SyntaxToken t)
                {
                    return (SyntaxToken?)t;
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

    internal SyntaxToken? GetLastTerminal()
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

        return (SyntaxToken?)(node as InternalSyntax.SyntaxToken);
    }

    public virtual Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return null!;
    }

    public virtual object? GetValue() => (int)Kind;

    public virtual string? GetValueText() => SyntaxFacts.GetSyntaxTokenText(Kind);

    protected static int CalculateWidth(GreenNode[] items)
    {
        if (items is null || items.Length == 0)
            return 0;

        if (items.Length == 1)
        {
            return items[0]?.Width ?? 0;
        }

        var items2 = items
            .Where(item => item is not null);

        var f1 = items2.First();
        var f2 = items2.Last();

        var value = items2.Sum(item => item.FullWidth);

        return value - f1.LeadingTrivia.Width - f2.TrailingTrivia.Width;
    }

    protected static int CalculateFullWidth(GreenNode[] items,
        SyntaxTriviaList? leadingTrivia = null, SyntaxTriviaList? trailingTrivia = null)
    {
        if (items is null || items.Length == 0)
            return 0;

        if (items.Length == 1)
        {
            return items[0]?.FullWidth ?? 0;
        }

        var value = items
            .Where(item => item is not null)
            .Sum(item => item.FullWidth);

        return value;
    }

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

        return new InternalSyntax.SyntaxList(newNodes.ToArray());
    }

    protected virtual GreenNode WithUpdatedChildren(GreenNode[] newChildren) => this;
}