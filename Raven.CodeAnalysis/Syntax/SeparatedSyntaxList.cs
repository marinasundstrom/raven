using System.Collections;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public struct SeparatedSyntaxList<TNode> : IEnumerable<TNode>
    where TNode : SyntaxNode
{
    internal readonly InternalSyntax.SeparatedSyntaxList Green;
    private readonly SyntaxNode _parent;
    private int _position;

    internal SeparatedSyntaxList(InternalSyntax.SeparatedSyntaxList greenList, SyntaxNode parent, int position)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;
    }

    public SeparatedSyntaxList(params SyntaxNodeOrToken[] items)
    {
        var p = items.Select(x => x.Green).ToArray();
        Green = new SeparatedSyntaxList(p);
    }

    public int Count => (Green.SlotCount + 1) / 2; // Elements are at even indices

    public int SeparatorCount => Green.SlotCount / 2;


    public TNode this[int index]
    {
        get
        {
            var node = Green[index * 2];
            var parent = _parent;
            var position = _position + Green.GetChildStartPosition(index * 2);

            return (TNode)SyntaxNodeCache.GetValue(node, (s) => s.CreateRed(parent, position));
        }
    }

    public SyntaxToken GetSeparator(int index)
    {
        if (index < 0 || index >= Count - 1)
            throw new IndexOutOfRangeException($"Invalid separator index: {index}");

        var separator = Green[index * 2 + 1] as InternalSyntax.SyntaxToken;
        return separator != null ? new SyntaxToken(separator, _parent) : default;
    }

    public IEnumerator<TNode> GetEnumerator()
    {
        return EnumerateItems()
            .Where(x => x.IsNode)
            .Select(x => x.Node)
            .OfType<TNode>()
            .GetEnumerator();
    }

    private IEnumerable<SyntaxNodeOrToken> EnumerateItems()
    {
        for (int i = 0; i < Green.SlotCount; i++)
        {
            var node = Green.GetSlot(i);
            yield return new SyntaxNodeOrToken(node, _parent, i, _position + Green.GetChildStartPosition(i));
        }
    }

    public IEnumerable<SyntaxNodeOrToken> GetWithSeparators()
    {
        return EnumerateItems();
    }

    public IEnumerable<SyntaxToken> GetSeparators()
    {
        return EnumerateItems()
                .Where(x => x.IsToken)
                .Select(x => x.Token)
                .OfType<SyntaxToken>();
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    public static bool operator ==(SeparatedSyntaxList<TNode> left, SeparatedSyntaxList<TNode>? right) => Equals(left, right);

    public static bool operator !=(SeparatedSyntaxList<TNode> left, SeparatedSyntaxList<TNode>? right) => !Equals(left, right);

    public static bool operator ==(SeparatedSyntaxList<TNode>? left, SeparatedSyntaxList<TNode>? right) => Equals(left, right);

    public static bool operator !=(SeparatedSyntaxList<TNode>? left, SeparatedSyntaxList<TNode>? right) => !Equals(left, right);

    public bool Equals(SeparatedSyntaxList<TNode> other)
    {
        return Green == other.Green && _parent == other._parent;
    }

    public override bool Equals(object? obj)
    {
        return obj is SeparatedSyntaxList<TNode> other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Green, _parent);
    }
}

public static partial class SyntaxFactory
{
    public static SeparatedSyntaxList<TNode> EmptySeparatedList<TNode>()
        where TNode : SyntaxNode
        => new SeparatedSyntaxList<TNode>(null, (SyntaxNode)null);

    public static SeparatedSyntaxList<TNode> SeparatedList<TNode>(params SyntaxNodeOrToken[] items)
        where TNode : SyntaxNode
        => new SeparatedSyntaxList<TNode>(items);
}