using System.Collections;
using System.Formats.Asn1;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxTriviaList : GreenNode, IEnumerable<SyntaxTrivia>
{
    public readonly static SyntaxTriviaList Empty = new SyntaxTriviaList([]);
    private readonly SyntaxTrivia[] _trivias;

    public SyntaxTriviaList(SyntaxTrivia[] trivias)
        : base(SyntaxKind.SyntaxList, trivias?.Length ?? 0, CalculateWidth(trivias), CalculateFullWidth(trivias))
    {
        _trivias = trivias ?? Array.Empty<SyntaxTrivia>();
    }

    public override GreenNode GetSlot(int index)
    {
        if (index < 0 || index >= SlotCount)
            throw new IndexOutOfRangeException($"Invalid slot index: {index}");
        return _trivias[index];
    }

    private static int CalculateFullWidth(SyntaxTrivia[] trivias) =>
        trivias?.Sum(trivia => trivia.FullWidth) ?? 0;


    /// <summary>
    /// Creates a new SyntaxTriviaList with the specified trivia added at the end.
    /// </summary>
    public SyntaxTriviaList Add(SyntaxTrivia trivia)
    {
        if (trivia == null) throw new ArgumentNullException(nameof(trivia));

        var newTrivias = _trivias.Concat(new[] { trivia }).ToArray();
        return new SyntaxTriviaList(newTrivias);
    }

    /// <summary>
    /// Creates a new SyntaxTriviaList with the specified trivia removed.
    /// </summary>
    public SyntaxTriviaList Remove(SyntaxTrivia trivia)
    {
        if (trivia == null) throw new ArgumentNullException(nameof(trivia));

        var newTrivias = _trivias.Where(t => !Equals(t, trivia)).ToArray();
        return new SyntaxTriviaList(newTrivias);
    }

    /// <summary>
    /// Creates a new SyntaxTriviaList from the given array of SyntaxTrivia.
    /// </summary>
    public static SyntaxTriviaList Create(SyntaxTrivia[] trivias)
    {
        if (trivias == null) throw new ArgumentNullException(nameof(trivias));
        return new SyntaxTriviaList(trivias);
    }

    public IEnumerator<SyntaxTrivia> GetEnumerator()
    {
        return _trivias.OfType<SyntaxTrivia>().GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}