using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.DocumentInfo;

namespace Raven.CodeAnalysis;

abstract class TextDocumentState
{
    private readonly DocumentAttributes _attributes;
    private readonly ITextAndVersionSource _textSource;
    private SourceText? _text;
    private VersionStamp _version;

    protected TextDocumentState(DocumentAttributes attributes, ITextAndVersionSource textSource)
    {
        _attributes = attributes;
        _textSource = textSource;
    }

    protected DocumentAttributes Attributes => _attributes;
    protected ITextAndVersionSource TextAndVersionSource => _textSource;

    public DocumentId Id => _attributes.Id;
    public string Name => _attributes.Name;
    public string? FilePath => _attributes.FilePath;

    private async Task EnsureTextLoadedAsync(CancellationToken cancellationToken)
    {
        if (_text != null)
            return;

        if (_attributes.Text is not null)
        {
            _text = _attributes.Text;
            _version = VersionStamp.Create();
            return;
        }

        if (_textSource.TryGetValue(out var cached))
        {
            _text = cached.Text;
            _version = cached.Version;
            return;
        }

        var loaded = await _textSource.TextLoader.LoadTextAndVersionAsync(Id, cancellationToken);
        _text = loaded.Text;
        _version = loaded.Version;
    }

    public async ValueTask<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
    {
        await EnsureTextLoadedAsync(cancellationToken);
        return _text!;
    }

    public async ValueTask<bool> TryGetTextAsync(out SourceText sourceText, CancellationToken cancellationToken = default)
    {
        await EnsureTextLoadedAsync(cancellationToken);
        if (_text is not null)
        {
            sourceText = _text;
            return true;
        }

        sourceText = default!;
        return false;
    }

    public async ValueTask<VersionStamp> GetVersionAsync(CancellationToken cancellationToken = default)
    {
        await EnsureTextLoadedAsync(cancellationToken);
        return _version;
    }
}
