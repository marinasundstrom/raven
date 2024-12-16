using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxList<TNode> : IEnumerable<TNode> where TNode : SyntaxNode
{
    public static readonly SyntaxList<TNode> Empty = new SyntaxList<TNode>(new InternalSyntax.SyntaxList([]), null, 0);

    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;

    public SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent, int position = 0)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;
    }

    public int Count => Green.SlotCount;

    public TNode this[int index]
    {
        get
        {
            var childGreenNode = Green[index];
            return (TNode)((InternalSyntax.SyntaxNode)childGreenNode).CreateRed(_parent, _position + Green.GetChildStartPosition(index));
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
    internal readonly GreenNode Green;
    private readonly SyntaxNode _parent;
    private readonly int _index;
    private readonly int _position;

    public SyntaxListItem(GreenNode node, SyntaxNode parent, int index, int position)
    {
        Green = node ?? throw new ArgumentNullException(nameof(node));
        _parent = parent;
        _index = index;
        _position = position;
    }

    public bool IsToken => Green is InternalSyntax.SyntaxToken;
    public bool IsNode => Green is InternalSyntax.SyntaxNode;

    public SyntaxToken Token => IsToken ? new SyntaxToken(Green as InternalSyntax.SyntaxToken, _parent, _position + Green.GetChildStartPosition(_index)) : default;
    public TNode? Node
    {
        get
        {
            return IsNode ? (TNode?)(object?)SyntaxNodeCache.GetValue(Green, (s) => s.CreateRed(_parent, _position + Green.GetChildStartPosition(_index))) : default;
        }
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxList<TNode> EmptyList<TNode>()
        where TNode : SyntaxNode
         => new SyntaxList<TNode>(new InternalSyntax.SyntaxList([]), null, 0);

    public static SyntaxList<TNode> SingletonList<TNode>(TNode node)
        where TNode : SyntaxNode
        => new SyntaxList<TNode>(new InternalSyntax.SyntaxList([
        node.Green
        ]), null, 0);

    public static SyntaxList<TNode> List<TNode>(params IEnumerable<TNode> nodes)
        where TNode : SyntaxNode
        => new SyntaxList<TNode>(new InternalSyntax.SyntaxList(
            nodes.Select(x => x.Green).ToArray()), null);
}