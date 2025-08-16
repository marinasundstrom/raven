using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public class TextDocument
{
    internal TextDocument(TextDocumentState state) => State = state;

    internal TextDocumentState State { get; }

    public DocumentId Id => State.Id;
    public string Name => State.Name;
    public ProjectId ProjectId => Id.ProjectId;
    public VersionStamp Version => State.Version;

    public virtual Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
        => State.GetTextAsync(cancellationToken);
}

