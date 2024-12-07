using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public class ChildSyntaxList : IEnumerable<ChildSyntaxListItem>
{
    private readonly SyntaxNode _node;

    public ChildSyntaxList(SyntaxNode node)
    {
        _node = node ?? throw new ArgumentNullException(nameof(node));
    }

    public ChildSyntaxListItem this[int index]
    {
        get
        {
            var childGreenNode = _node.Green.GetSlot(index);
            return new ChildSyntaxListItem(childGreenNode, _node);
        }
    }

    public IEnumerator<ChildSyntaxListItem> GetEnumerator()
    {
        for (int i = 0; i < _node.Green.SlotCount; i++)
        {
            yield return this[i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public class ChildSyntaxListItem
{
    private readonly GreenNode _node;
    private readonly SyntaxNode _parent;

    public ChildSyntaxListItem(GreenNode node, SyntaxNode parent)
    {
        _node = node ?? throw new ArgumentNullException(nameof(node));
        _parent = parent;
    }

    public bool IsToken => _node is InternalSyntax.SyntaxToken;
    public bool IsNode => _node is InternalSyntax.SyntaxNode;
    public SyntaxToken Token => _node as InternalSyntax.SyntaxToken != null ? new SyntaxToken(_node as InternalSyntax.SyntaxToken, _parent) : default;
    public SyntaxNode Node => _node is InternalSyntax.SyntaxNode ? SyntaxFactory.CreateWrapper(_node as InternalSyntax.SyntaxNode, _parent) : null;

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

    public bool AsNode(out SyntaxNode? node)
    {
        if (IsNode)
        {
            node = Node;

            return true;
        }

        node = null;
        return false;
    }
}