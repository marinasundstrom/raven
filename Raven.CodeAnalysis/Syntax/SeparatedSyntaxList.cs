using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

/*
public class SyntaxList<T> : IEnumerable<SyntaxListItem<T>>
    where T : SyntaxNode
{
    private readonly InternalSyntax.SyntaxList _greenList;
    private readonly SyntaxNode _parent;

    public SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent)
    {
        _greenList = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
    }

    public int Count => _greenList.SlotCount;

    public SyntaxListItem<T> this[int index] => new SyntaxListItem<T>(_greenList[index], _parent);

    public IEnumerator<SyntaxListItem<T>> GetEnumerator()
    {
        for (int i = 0; i < Count; i++)
        {
            yield return this[i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public class SyntaxListItem<T>
    where T : SyntaxNode
{
    private readonly InternalSyntax.SyntaxListItem _item;
    private readonly SyntaxNode _parent;

    public SyntaxListItem(InternalSyntax.SyntaxListItem item, SyntaxNode parent)
    {
        _item = item;
        _parent = parent;
    }

    /// <summary>
    /// Indicates whether the item is a SyntaxToken.
    /// </summary>
    public bool IsToken => _item.IsToken;

    /// <summary>
    /// Indicates whether the item is a SyntaxNode.
    /// </summary>
    public bool IsNode => _item.IsNode;

    /// <summary>
    /// Gets the SyntaxToken if the item is a token; otherwise, null.
    /// </summary>
    public SyntaxToken Token => new SyntaxToken(_item.Token, _parent);

    /// <summary>
    /// Gets the SyntaxNode if the item is a node; otherwise, null.
    /// </summary>
    public T NodeSyntax => default!; // new SyntaxNode(_item.NodeSyntax, _parent);

    /// <summary>
    /// Gets the underlying GreenNode.
    /// </summary>
    public GreenNode Node => _item.Node;
}
*/


public class SeparatedSyntaxList : IEnumerable<SyntaxListItem>
{
    private readonly InternalSyntax.SyntaxList _greenList;
    private readonly SyntaxNode _parent;

    public SeparatedSyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent)
    {
        _greenList = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
    }

    public int ElementCount => (_greenList.SlotCount + 1) / 2; // Elements are at even indices

    public SyntaxListItem this[int index] => new SyntaxListItem(_greenList[index * 2].Node, _parent);

    public SyntaxToken GetSeparator(int index)
    {
        if (index < 0 || index >= ElementCount - 1)
            throw new IndexOutOfRangeException($"Invalid separator index: {index}");

        var separator = _greenList[index * 2 + 1].Node as InternalSyntax.SyntaxToken;
        return separator != null ? new SyntaxToken(separator, _parent) : default;
    }

    public IEnumerator<SyntaxListItem> GetEnumerator()
    {
        for (int i = 0; i < ElementCount; i++)
        {
            yield return this[i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}