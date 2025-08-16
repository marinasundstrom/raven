using System.Threading;
using System.Threading.Tasks;
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
        var source = State.TextAndVersionSource;
        if (source.TryGetValue(out var tav))
            return tav.Text;

        var loaded = await source.TextLoader.LoadTextAndVersionAsync(cancellationToken).ConfigureAwait(false);
        if (source is TextAndVersionSource concrete)
            concrete.SetValue(loaded);
        return loaded.Text;
    }

    public Task<bool> TryGetTextAsync(out SourceText text, CancellationToken cancellationToken = default)
    {
        var source = State.TextAndVersionSource;
        if (source.TryGetValue(out var tav))
        {
            text = tav.Text;
            return Task.FromResult(true);
        }

        text = null!;
        return Task.FromResult(false);
    }
}
