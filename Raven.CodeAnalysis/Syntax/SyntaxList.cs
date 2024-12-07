using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxList : IEnumerable<SyntaxNode>
{
    private readonly InternalSyntax.SyntaxList _greenList;
    private readonly SyntaxNode _parent;

    public SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent)
    {
        _greenList = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
    }

    public int Count => _greenList.SlotCount;

    public SyntaxNode this[int index]
    {
        get
        {
            var childGreenNode = _greenList[index].Node;
            return childGreenNode.CreateRed(_parent);
        }
    }

    public IEnumerator<SyntaxNode> GetEnumerator()
    {
        for (int i = 0; i < Count; i++)
        {
            yield return this[i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public class SyntaxListItem
{
    private readonly GreenNode _node;
    private readonly SyntaxNode _parent;

    public SyntaxListItem(GreenNode node, SyntaxNode parent)
    {
        _node = node ?? throw new ArgumentNullException(nameof(node));
        _parent = parent;
    }

    public bool IsToken => _node is InternalSyntax.SyntaxToken;
    public bool IsNode => _node is InternalSyntax.SyntaxNode;
    public SyntaxToken Token => _node as InternalSyntax.SyntaxToken != null ? new SyntaxToken(_node as InternalSyntax.SyntaxToken, _parent) : default;
    public SyntaxNode NodeSyntax => _node is InternalSyntax.SyntaxNode ? _node.CreateRed(_parent) : null;
}