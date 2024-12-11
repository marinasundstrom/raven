using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxList : IEnumerable<SyntaxNode>
{
    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;

    public SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent, int position)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;
    }

    public int Count => Green.SlotCount;

    public SyntaxNode this[int index]
    {
        get
        {
            var childGreenNode = Green[index];
            return childGreenNode.CreateRed(_parent, _position + Green.GetChildStartPosition(index));
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
    public SyntaxToken Token => Green as InternalSyntax.SyntaxToken != null ? new SyntaxToken(Green as InternalSyntax.SyntaxToken, _parent, _position + Green.GetChildStartPosition(_index)) : default;
    public SyntaxNode Node => Green is InternalSyntax.SyntaxNode ? Green.CreateRed(_parent, _position + Green.GetChildStartPosition(_index)) : null;
}