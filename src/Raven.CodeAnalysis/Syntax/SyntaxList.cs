using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxList : IEnumerable<SyntaxNode>
{
    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;

    internal SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent, int position)
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
            var parent = _parent;
            var position = _position + Green.GetChildStartPosition(index);

            return SyntaxNodeCache.GetValue(childGreenNode, (s) => s.CreateRed(parent, position));
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

    public static bool operator ==(SyntaxList left, SyntaxList? right) => Equals(left, right);

    public static bool operator !=(SyntaxList left, SyntaxList? right) => !Equals(left, right);

    public static bool operator ==(SyntaxList? left, SyntaxList? right) => Equals(left, right);

    public static bool operator !=(SyntaxList? left, SyntaxList? right) => !Equals(left, right);

    public bool Equals(SyntaxList other)
    {
        return Green == other.Green && _parent == other._parent;
    }

    public override bool Equals(object? obj)
    {
        return obj is SyntaxList other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Green, _parent);
    }

    public struct SyntaxListItem
    {
        internal readonly GreenNode Green;
        private readonly SyntaxNode _parent;
        private readonly int _index;
        private readonly int _position;

        internal SyntaxListItem(GreenNode node, SyntaxNode parent, int index, int position)
        {
            Green = node ?? throw new ArgumentNullException(nameof(node));
            _parent = parent;
            _index = index;
            _position = position;
        }

        public bool IsToken => Green is InternalSyntax.SyntaxToken;
        public bool IsNode => Green is InternalSyntax.SyntaxNode;

        public SyntaxToken Token => Green as InternalSyntax.SyntaxToken != null
            ? new SyntaxToken(Green as InternalSyntax.SyntaxToken, _parent,
                _position + Green.GetChildStartPosition(_index))
            : default;

        public SyntaxNode? Node
        {
            get
            {
                return Green is InternalSyntax.SyntaxNode
                    ? Green.CreateRed(_parent, _position + Green.GetChildStartPosition(_index))
                    : null;
            }
        }
    }
}