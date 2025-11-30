namespace Raven.CodeAnalysis.Syntax;

public readonly struct SyntaxTokenList : IEnumerable<SyntaxToken>
{
    public static readonly SyntaxTokenList Empty = new(new InternalSyntax.SyntaxList([]), null, 0);

    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;
    private readonly TextSpan _span;
    private readonly TextSpan _fullSpan;

    internal SyntaxTokenList(InternalSyntax.SyntaxList greenList, SyntaxNode parent, int position)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;

        greenList.ComputeSpanAndFullSpan(position, out _span, out _fullSpan);
    }

    public int Count => Green.SlotCount;

    public TextSpan Span => _span;

    public TextSpan FullSpan => _fullSpan;

    public SyntaxToken this[int index]
    {
        get
        {
            var childGreenNode = (InternalSyntax.SyntaxToken)Green[index];
            var parent = _parent;
            var position = _position + Green.GetChildStartPosition(index);

            return new Syntax.SyntaxToken(childGreenNode, parent, position);
        }
    }

    public SyntaxTokenList Add(SyntaxToken syntaxToken)
    {
        return new SyntaxTokenList(this.Green.Add(syntaxToken.Green), null, 0);
    }

    public IEnumerator<SyntaxToken> GetEnumerator()
    {
        for (int i = 0; i < Count; i++)
        {
            yield return this[i];
        }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();

    public static bool operator ==(SyntaxTokenList left, SyntaxTokenList? right) => Equals(left, right);

    public static bool operator !=(SyntaxTokenList left, SyntaxTokenList? right) => !Equals(left, right);

    public static bool operator ==(SyntaxTokenList? left, SyntaxTokenList? right) => Equals(left, right);

    public static bool operator !=(SyntaxTokenList? left, SyntaxTokenList? right) => !Equals(left, right);

    public bool Equals(SyntaxTokenList other)
    {
        return Green == other.Green && _parent == other._parent;
    }

    public override bool Equals(object? obj)
    {
        return obj is SyntaxTokenList other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Green, _parent);
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxTokenList TokenList() => SyntaxTokenList.Empty;

    public static SyntaxTokenList TokenList(params IEnumerable<SyntaxToken> tokens) => new SyntaxTokenList(InternalSyntax.SyntaxFactory.TokenList(tokens.Select(x => x.Green).ToArray()), null, 0);
}
