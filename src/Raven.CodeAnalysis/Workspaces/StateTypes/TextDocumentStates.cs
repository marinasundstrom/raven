namespace Raven.CodeAnalysis;

sealed class TextDocumentStates<TState> where TState : TextDocumentState
{
    public TextDocumentStates(IEnumerable<DocumentState> enumerable)
    {

    }

    //private sealed class DocumentIdComparer : IComparer<DocumentId?>

    public IReadOnlyList<DocumentId> Ids { get; }
}