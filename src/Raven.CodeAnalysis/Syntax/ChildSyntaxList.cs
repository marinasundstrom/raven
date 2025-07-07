using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public class ChildSyntaxList : IEnumerable<ChildSyntaxListItem>
{
    private readonly SyntaxNode _node;
    private ChildSyntaxListItem[]? children;

    public ChildSyntaxList(SyntaxNode node)
    {
        _node = node ?? throw new ArgumentNullException(nameof(node));
    }

    public ChildSyntaxListItem this[int index]
    {
        get
        {
            if (children is null)
            {
                InitializeChildren();
            }
            return children[index];
        }
    }

    private void InitializeChildren()
    {
        int position = _node.Position;

        var nodeGreen = _node.Green;

        var tempChildren = new List<ChildSyntaxListItem>();
        for (int index = 0; index < nodeGreen.SlotCount; index++)
        {
            var childGreenNode = nodeGreen.GetSlot(index);
            if (childGreenNode is InternalSyntax.SyntaxList syntaxList)
            {
                for (int i = 0; i < syntaxList.SlotCount; i++)
                {
                    var listItemGreenNode = syntaxList.GetSlot(i);
                    if (listItemGreenNode is not null)
                    {
                        tempChildren.Add(new ChildSyntaxListItem(listItemGreenNode, _node, position, syntaxList));

                        position += listItemGreenNode.FullWidth;
                    }
                }
            }
            else
            {
                if (childGreenNode is not null)
                {
                    tempChildren.Add(new ChildSyntaxListItem(childGreenNode, _node, position));

                    position += childGreenNode.FullWidth;
                }
            }
        }
        children = tempChildren.ToArray();
    }

    public IEnumerator<ChildSyntaxListItem> GetEnumerator()
    {
        if (children is null)
        {
            InitializeChildren();
        }
        return children
            .AsEnumerable()
            .GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public class ChildSyntaxListItem
{
    private readonly GreenNode _itemGreenNode;
    private readonly SyntaxNode _parentNode;
    private readonly int _position;
    private readonly InternalSyntax.SyntaxList? _parentListGreen;
    private SyntaxToken? _token;
    private SyntaxNode? _node;

    internal ChildSyntaxListItem(GreenNode itemGreenNode, SyntaxNode parentNode, int position, InternalSyntax.SyntaxList? parentListGreen = null)
    {
        _itemGreenNode = itemGreenNode ?? throw new ArgumentNullException(nameof(itemGreenNode));
        _parentNode = parentNode;
        _position = position;
        _parentListGreen = parentListGreen;
    }

    public bool IsToken => _itemGreenNode is InternalSyntax.SyntaxToken;
    public bool IsNode => _itemGreenNode is InternalSyntax.SyntaxNode;

    public SyntaxToken AsToken() => _token ??= IsToken ? new SyntaxToken(_itemGreenNode as InternalSyntax.SyntaxToken, _parentNode, _position) : default;
    public SyntaxNode? AsNode() => _node ??= IsNode ? (SyntaxNode?)SyntaxNodeCache.GetValue(_itemGreenNode, (s) => s.CreateRed(_parentNode, _position)) : null;

    public SyntaxNode Parent => _parentNode;

    internal InternalSyntax.SyntaxList? ParentListGreen => _parentListGreen;

    public bool TryGetToken(out SyntaxToken token)
    {
        if (IsToken)
        {
            token = AsToken();

            return true;
        }

        token = default;
        return false;
    }

    public bool TryGetNode([NotNullWhen(true)] out SyntaxNode? node)
    {
        if (IsNode)
        {
            node = AsNode()!;

            return true;
        }

        node = null;
        return false;
    }

    public static implicit operator SyntaxNodeOrToken(ChildSyntaxListItem childSyntaxListItem)
    {
        return childSyntaxListItem.IsToken
            ? new SyntaxNodeOrToken(childSyntaxListItem.AsToken())
            : new SyntaxNodeOrToken(childSyntaxListItem.AsNode()!);
    }
}