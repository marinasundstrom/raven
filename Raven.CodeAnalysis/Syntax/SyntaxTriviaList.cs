using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTriviaList : IEnumerable<SyntaxTrivia>
{
    public static SyntaxTriviaList Empty => default!;

    internal readonly InternalSyntax.SyntaxTriviaList Green;
    private readonly SyntaxToken _parent;

    public SyntaxTriviaList(SyntaxToken parent, InternalSyntax.SyntaxTriviaList greenList)
    {
        Green = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
    }

    public int Width => Green.Width;

    public int Count => Green.SlotCount;

    public SyntaxTrivia this[int index]
    {
        get
        {
            var triviaGreenNode = Green.GetSlot(index) as InternalSyntax.SyntaxTrivia;
            return new SyntaxTrivia(triviaGreenNode, _parent);
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


    public SyntaxTriviaList AddTrivia(SyntaxTrivia trivia)
    {
        var newGreenList = Green.Add(trivia.Green); // Assumes a method Add exists in InternalSyntax.SyntaxTriviaList
        return new SyntaxTriviaList(_parent, newGreenList);
    }

    public SyntaxTriviaList RemoveTrivia(SyntaxTrivia trivia)
    {
        var greenNodes = Green.Select((node, index) => (node, index))
                                   .Where(pair => !Equals(pair.node, trivia.Green))
                                   .Select(pair => pair.node)
                                   .ToArray();

        var newGreenList = InternalSyntax.SyntaxTriviaList.Create(greenNodes); // Assumes a Create method exists to build a new list
        return new SyntaxTriviaList(_parent, newGreenList);
    }
}