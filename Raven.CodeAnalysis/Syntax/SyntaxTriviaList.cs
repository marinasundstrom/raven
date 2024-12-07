using System.Collections;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTriviaList : IEnumerable<SyntaxTrivia>
{
    public static SyntaxTriviaList Empty => default!;

    private readonly InternalSyntax.SyntaxTriviaList _greenList;
    private readonly SyntaxToken _parent;

    public SyntaxTriviaList(SyntaxToken parent, InternalSyntax.SyntaxTriviaList greenList)
    {
        _greenList = greenList ?? throw new ArgumentNullException(nameof(greenList));
        _parent = parent;
    }

    public int Count => _greenList.SlotCount;

    public SyntaxTrivia this[int index]
    {
        get
        {
            var triviaGreenNode = _greenList.GetSlot(index) as InternalSyntax.SyntaxTrivia;
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
}