using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public abstract class TextDocument
{
    internal TextDocument(Project project, TextDocumentState state)
    {
        Project = project;
        State = state;
    }

    public DocumentId Id => State.Id;

    public string Name => State.Name;

    public Project Project { get; }

    internal TextDocumentState State { get; }

    public async Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
    {
        return await State.GetTextAsync(cancellationToken);
    }

    public Task<bool> TryGetTextAsync(out SourceText text, CancellationToken cancellationToken = default)
    {
        return State.TryGetTextAsync(out text, cancellationToken).AsTask();
    }
}
