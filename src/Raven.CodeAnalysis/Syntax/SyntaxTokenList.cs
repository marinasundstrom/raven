namespace Raven.CodeAnalysis.Syntax;

public readonly struct SyntaxTokenList : IEnumerable<SyntaxToken>
{
    internal readonly InternalSyntax.SyntaxList Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;

    internal SyntaxTokenList(InternalSyntax.SyntaxList greenList, SyntaxNode parent, int position)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;
    }

    public int Count => Green.SlotCount;

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
    public static SyntaxTokenList TokenList(params IEnumerable<SyntaxToken> tokens) => new SyntaxTokenList(InternalSyntax.SyntaxFactory.TokenList(tokens.Select(x => x.Green).ToArray()), null, 0);
}