using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxList<TNode> : IEnumerable<TNode> where TNode : SyntaxNode
{
    public static readonly SyntaxList<TNode> Empty = default;

    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;

    public SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
    }

    public int Count => Green.SlotCount;

    public TNode this[int index]
    {
        get
        {
            var childGreenNode = Green[index].Node;
            return (TNode)((InternalSyntax.SyntaxNode)childGreenNode).CreateRed(_parent);
        }
    }

    public IEnumerator<TNode> GetEnumerator()
    {
        for (int i = 0; i < Count; i++)
        {
            yield return this[i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public class SyntaxListItem<TNode>
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
    public TNode? NodeSyntax => _node is TNode ? (TNode)(object)_node.CreateRed(_parent) : default;
}

public static partial class SyntaxFactory
{
    public static SyntaxList<TNode> EmptyList<TNode>()
        where TNode : SyntaxNode
         => new SyntaxList<TNode>(new InternalSyntax.SyntaxList([], 0), null);

    public static SyntaxList<TNode> SingletonList<TNode>(TNode node)
        where TNode : SyntaxNode
        => new SyntaxList<TNode>(new InternalSyntax.SyntaxList([
        new InternalSyntax.SyntaxListItem(node.Green)
        ], 0), null);

    public static SyntaxList<TNode> List<TNode>(params IEnumerable<TNode> nodes)
        where TNode : SyntaxNode
        => new SyntaxList<TNode>(new InternalSyntax.SyntaxList(
            nodes.Select(node => new InternalSyntax.SyntaxListItem(node.Green)).ToArray(), 0), null);
}