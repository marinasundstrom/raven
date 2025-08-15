using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTriviaList : IEnumerable<SyntaxTrivia>, IEquatable<SyntaxTriviaList>
{
    public static SyntaxTriviaList Empty = new SyntaxTriviaList([]);
    private readonly int _position;
    internal readonly InternalSyntax.SyntaxTriviaList Green;
    private readonly SyntaxToken _parent;
    private readonly TextSpan _span;
    private readonly TextSpan _fullSpan;

    public SyntaxTriviaList()
    {
        Green = new InternalSyntax.SyntaxTriviaList([], null);
        _parent = default!;
        _position = 0;

        var greenList = Green;
        var position = _position;

        if (greenList.SlotCount == 0)
        {
            _span = _fullSpan = new TextSpan(position, 0);
        }
        else
        {
            var firstGreen = greenList[0];
            var lastGreen = greenList[greenList.SlotCount - 1];

            int spanStart = position + firstGreen.GetLeadingTriviaWidth();
            int fullWidth = greenList.FullWidth;
            int spanEnd = position + fullWidth - lastGreen.GetTrailingTriviaWidth();
            int spanWidth = spanEnd - spanStart;

            _span = new TextSpan(spanStart, spanWidth);
            _fullSpan = new TextSpan(position, fullWidth);
        }
    }

    internal SyntaxTriviaList(params IEnumerable<SyntaxTrivia> trivias)
    {
        Green = new InternalSyntax.SyntaxTriviaList(trivias.Select(x => x.Green).ToArray());
        _parent = default;

        var greenList = Green;
        var position = _position;

        if (greenList.SlotCount == 0)
        {
            _span = _fullSpan = new TextSpan(position, 0);
        }
        else
        {
            var firstGreen = greenList[0];
            var lastGreen = greenList[greenList.SlotCount - 1];

            int spanStart = position + firstGreen.GetLeadingTriviaWidth();
            int fullWidth = greenList.FullWidth;
            int spanEnd = position + fullWidth - lastGreen.GetTrailingTriviaWidth();
            int spanWidth = spanEnd - spanStart;

            _span = new TextSpan(spanStart, spanWidth);
            _fullSpan = new TextSpan(position, fullWidth);
        }
    }

    internal SyntaxTriviaList(SyntaxToken parent, InternalSyntax.SyntaxTriviaList greenList, int position = 0)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;

        if (greenList.SlotCount == 0)
        {
            _span = _fullSpan = new TextSpan(position, 0);
        }
        else
        {
            var firstGreen = greenList[0];
            var lastGreen = greenList[greenList.SlotCount - 1];

            int spanStart = position + firstGreen.GetLeadingTriviaWidth();
            int fullWidth = greenList.FullWidth;
            int spanEnd = position + fullWidth - lastGreen.GetTrailingTriviaWidth();
            int spanWidth = spanEnd - spanStart;

            _span = new TextSpan(spanStart, spanWidth);
            _fullSpan = new TextSpan(position, fullWidth);
        }
    }

    public int Width => Green.Width;

    public int Count => Green.SlotCount;

    public SyntaxTrivia this[int index]
    {
        get
        {
            var triviaGreenNode = Green.GetSlot(index) as InternalSyntax.SyntaxTrivia;
            return new SyntaxTrivia(triviaGreenNode!, _parent, _position + Green.GetChildStartPosition(index));
        }
    }

    public IEnumerator<SyntaxTrivia> GetEnumerator()
    {
        for (int i = 0; i < Count; i++)
        {
            yield return this[i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    public SyntaxTriviaList Add(SyntaxTrivia trivia)
    {
        var newGreenList = Green.Add(trivia.Green); // Assumes a method Add exists in InternalSyntax.SyntaxTriviaList
        return new SyntaxTriviaList(_parent, newGreenList);
    }

    public SyntaxTriviaList Remove(SyntaxKind kind)
    {
        var greenNodes = Green.Select((node, index) => (node, index))
                                   .Where(pair => !Equals(pair.node.Kind, kind))
                                   .Select(pair => pair.node)
                                   .ToArray();

        var newGreenList = InternalSyntax.SyntaxTriviaList.Create(greenNodes); // Assumes a Create method exists to build a new list
        return new SyntaxTriviaList(_parent, newGreenList);
    }

    public SyntaxTriviaList Remove(SyntaxTrivia trivia)
    {
        var greenNodes = Green.Select((node, index) => (node, index))
                                   .Where(pair => !Equals(pair.node, trivia.Green))
                                   .Select(pair => pair.node)
                                   .ToArray();

        var newGreenList = InternalSyntax.SyntaxTriviaList.Create(greenNodes); // Assumes a Create method exists to build a new list
        return new SyntaxTriviaList(_parent, newGreenList);
    }

    public override string ToString()
    {
        return string.Concat(this.ToArray());
    }

    public override bool Equals(object? obj)
    => obj is SyntaxTriviaList other && Equals(other);

    public bool Equals(SyntaxTriviaList other)
        => Green == other.Green && _parent.Equals(other._parent) && _position == other._position;

    public override int GetHashCode()
        => HashCode.Combine(Green, _parent, _position);

    public static bool operator ==(SyntaxTriviaList left, SyntaxTriviaList right)
    => left.Equals(right);

    public static bool operator !=(SyntaxTriviaList left, SyntaxTriviaList right)
        => !left.Equals(right);
}

public static partial class SyntaxFactory
{
    public static SyntaxTriviaList TriviaList(params IEnumerable<SyntaxTrivia> trivias) => new SyntaxTriviaList(trivias);
}
