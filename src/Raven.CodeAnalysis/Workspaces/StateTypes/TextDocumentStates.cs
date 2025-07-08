using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

sealed class TextDocumentStates<TState> where TState : TextDocumentState
{
    public TextDocumentStates(IImmutableList<TState> states)
    {
        Ids = [.. states.Select(x => x.Id)];
        States = states.ToImmutableSortedDictionary(x => x.Id, x => x);
    }

    //private sealed class DocumentIdComparer : IComparer<DocumentId?>

    public static readonly TextDocumentStates<TState> Empty = new TextDocumentStates<TState>([]);

    public IReadOnlyList<DocumentId> Ids { get; }

    public ImmutableSortedDictionary<DocumentId, TState> States { get; }

    public int Count => Ids.Count;

    public bool IsEmpty => Ids.Count == 0;
}