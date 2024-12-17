using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

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

    public abstract GreenNode GetSlot(int index);

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
        if (items is null)
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

    protected static int CalculateFullWidth(GreenNode[] items,
        SyntaxTriviaList? leadingTrivia = null, SyntaxTriviaList? trailingTrivia = null)
    {
        if (items is null)
            return 0;

        var width = CalculateWidth(items);

        return (leadingTrivia?.Width ?? 0) + width + (trailingTrivia?.Width ?? 0);
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

    protected virtual GreenNode WithUpdatedChildren(GreenNode[] newChildren) => this;
}