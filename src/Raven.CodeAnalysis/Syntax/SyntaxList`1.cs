using System.Collections;
using System.Diagnostics;
using System.Security.Cryptography.X509Certificates;
using System.Text;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public struct SyntaxList<TNode> : IEnumerable<TNode>, IReadOnlyCollection<TNode>, IReadOnlyList<TNode>
    where TNode : SyntaxNode
{
    public static readonly SyntaxList<TNode> Empty = new SyntaxList<TNode>(new InternalSyntax.SyntaxList([]), null, 0);

    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;
    private readonly TextSpan _span;
    private readonly TextSpan _fullSpan;

    public SyntaxList()
    {
        Green = new InternalSyntax.SyntaxList([], null);
        _parent = null;
        _position = 0;
    }

    internal SyntaxList(InternalSyntax.SyntaxList greenList, SyntaxNode parent, int position = 0)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;

        GreenSpanHelper.ComputeSpanAndFullSpan(greenList, position, out _span, out _fullSpan);
    }

    public SyntaxList<TNode> Add(TNode node)
    {
        return new SyntaxList<TNode>(Green.Add(node.Green), null);
    }

    public SyntaxList<TNode> Remove(TNode node)
    {
        return new SyntaxList<TNode>(Green.Remove(node.Green), null);
    }

    public int IndexOf(Func<TNode, bool> predicate)
    {
        for (int i = 0; i < Count; i++)
        {
            var node = this[i];
            if (predicate(node))
                return i;
        }
        return -1;
    }

    public int IndexOf(TNode node)
    {
        for (int i = 0; i < Count; i++)
        {
            if (this[i].Equals(node))
                return i;
        }
        return -1;
    }

    public int Count => Green.SlotCount;

    public TextSpan Span => _span;

    public TextSpan FullSpan => _fullSpan;

    public TNode this[int index]
    {
        get
        {
            var childGreenNode = Green[index];
            return (TNode)((InternalSyntax.SyntaxNode)childGreenNode).CreateRed(_parent,
                _position + Green.GetChildStartPosition(index));
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

    public static bool operator ==(SyntaxList<TNode> left, SyntaxList<TNode>? right) => Equals(left, right);

    public static bool operator !=(SyntaxList<TNode> left, SyntaxList<TNode>? right) => !Equals(left, right);

    public static bool operator ==(SyntaxList<TNode>? left, SyntaxList<TNode>? right) => Equals(left, right);

    public static bool operator !=(SyntaxList<TNode>? left, SyntaxList<TNode>? right) => !Equals(left, right);

    public bool Equals(SyntaxList<TNode> other)
    {
        return Green == other.Green && _parent == other._parent;
    }

    public override bool Equals(object? obj)
    {
        return obj is SyntaxList<TNode> other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Green, _parent);
    }

    private struct SyntaxListItem<TNode>
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

        public SyntaxToken Token => IsToken
            ? new SyntaxToken(Green as InternalSyntax.SyntaxToken, _parent,
                _position + Green.GetChildStartPosition(_index))
            : default;

        public TNode? Node
        {
            get
            {
                var parent = _parent;
                var position = _position + Green.GetChildStartPosition(_index);

                return IsNode
                    ? (TNode?)(object?)SyntaxNodeCache.GetValue(Green,
                        (s) => s.CreateRed(parent, position))
                    : default;
            }
        }
    }

    public string GetDebuggerDisplay()
        => ToFullString();

    public string ToFullString()
    {
        var sb = new StringBuilder();
        foreach (var node in this)
            sb.Append(node.ToFullString());
        return sb.ToString();
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