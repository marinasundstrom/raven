using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTriviaList : IEnumerable<SyntaxTrivia>
{
    public static SyntaxTriviaList Empty = new SyntaxTriviaList([]);
    private int _position;
    internal readonly InternalSyntax.SyntaxTriviaList Green;
    private readonly SyntaxToken _parent;

    public SyntaxTriviaList()
    {
        Green = new InternalSyntax.SyntaxTriviaList([], null);
        _parent = default!;
        _position = 0;
    }

    internal SyntaxTriviaList(params IEnumerable<SyntaxTrivia> trivias)
    {
        Green = new InternalSyntax.SyntaxTriviaList(trivias.Select(x => x.Green).ToArray());
        _parent = default;
    }

    internal SyntaxTriviaList(SyntaxToken parent, InternalSyntax.SyntaxTriviaList greenList, int position = 0)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
        _position = position;
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
}

public static partial class SyntaxFactory
{
    public static SyntaxTriviaList TriviaList(params IEnumerable<SyntaxTrivia> trivias) => new SyntaxTriviaList(trivias);
}