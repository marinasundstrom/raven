namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxTriviaList : GreenNode
{
    private readonly SyntaxTrivia[] _trivias;

    public SyntaxTriviaList(SyntaxTrivia[] trivias = null, int startPosition = 0)
        : base(SyntaxKind.SyntaxList, trivias?.Length ?? 0, CalculateFullWidth(trivias), startPosition: startPosition)
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
}
