using System.Collections;
using System.ComponentModel;
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

        var tempChildren = new List<ChildSyntaxListItem>();
        for (int index = 0; index < _node.Green.SlotCount; index++)
        {
            var childGreenNode = _node.Green.GetSlot(index);
            if (childGreenNode is InternalSyntax.SyntaxList
                || childGreenNode is InternalSyntax.SeparatedSyntaxList)
            {
                var syntaxList = childGreenNode;

                for (int i = 0; i < syntaxList.SlotCount; i++)
                {
                    var child = syntaxList.GetSlot(i);
                    if (child is not null)
                    {
                        tempChildren.Add(new ChildSyntaxListItem(child, _node, position));

                        position += child.FullWidth;
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
    private readonly GreenNode _node;
    private readonly SyntaxNode _parent;
    private readonly int _position;

    public ChildSyntaxListItem(GreenNode node, SyntaxNode parent, int position)
    {
        _node = node ?? throw new ArgumentNullException(nameof(node));
        _parent = parent;
        _position = position;
    }

    public bool IsToken => _node is InternalSyntax.SyntaxToken;
    public bool IsNode => _node is InternalSyntax.SyntaxNode;
    public SyntaxToken Token => IsToken ? new SyntaxToken(_node as InternalSyntax.SyntaxToken, _parent, _position) : default;
    public SyntaxNode? Node => IsNode ? _node.CreateRed(_parent, _position) : null;

    public bool AsToken(out SyntaxToken token)
    {
        if (IsToken)
        {
            token = Token;

            return true;
        }

        token = default;
        return false;
    }

    public bool AsNode([NotNullWhen(true)] out SyntaxNode? node)
    {
        if (IsNode)
        {
            node = Node!;

            return true;
        }

        node = null;
        return false;
    }
}