using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal class TextDocumentState
{
    protected DocumentInfo Info { get; }
    private readonly ITextAndVersionSource _textSource;

    public TextDocumentState(DocumentInfo info, VersionStamp version)
    {
        Info = info;
        Version = version;
        _textSource = new TextAndVersionSource(ct => info.TextLoader.LoadTextAndVersionAsync(info.Id, ct));
    }

    public DocumentId Id => Info.Id;
    public string Name => Info.Name;
    public VersionStamp Version { get; }

    public virtual async Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
        => (await _textSource.GetValueAsync(cancellationToken).ConfigureAwait(false)).Text;

    protected virtual TextDocumentState With(DocumentInfo info, VersionStamp version)
        => new TextDocumentState(info, version);

    public virtual TextDocumentState WithText(SourceText text)
        => With(Info.WithText(text), Version.GetNewerVersion());

    public virtual TextDocumentState WithName(string name)
        => With(Info.WithName(name), Version);
}

internal sealed class DocumentState : TextDocumentState
{
    public DocumentState(DocumentInfo info, VersionStamp version) : base(info, version) { }

    protected override TextDocumentState With(DocumentInfo info, VersionStamp version)
        => new DocumentState(info, version);

    public new DocumentState WithText(SourceText text)
        => (DocumentState)base.WithText(text);

    public new DocumentState WithName(string name)
        => (DocumentState)base.WithName(name);

    internal new DocumentInfo Info => base.Info;
}

