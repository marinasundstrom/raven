using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.DocumentInfo;

namespace Raven.CodeAnalysis;

abstract class TextDocumentState
{
    private readonly DocumentAttributes _attributes;
    private readonly ITextAndVersionSource _textSource;

    public TextDocumentState(DocumentAttributes attributes, ITextAndVersionSource textSource)
    {
        _attributes = attributes;
        _textSource = textSource;
    }

    protected DocumentAttributes Info => _attributes;

    public DocumentId Id => _attributes.Id;
    public string Name => _attributes.Name;
    public string? FilePath => _attributes.FilePath;
    public ITextAndVersionSource TextAndVersionSource => _textSource;

    public SourceText Text
    {
        get
        {
            if (_textSource.TryGetValue(out var tav))
                return tav.Text;

            return _attributes.Text ?? SourceText.From(string.Empty);
        }
    }

    public ValueTask<SourceText> GetTextAsync(CancellationToken cancellationToken)
    {
        if (_textSource.TryGetValue(out var tav))
            return ValueTask.FromResult(tav.Text);

        return new ValueTask<SourceText>(LoadAsync(cancellationToken));
    }

    private async Task<SourceText> LoadAsync(CancellationToken cancellationToken)
    {
        var tav = await _textSource.TextLoader.LoadTextAndVersionAsync(cancellationToken).ConfigureAwait(false);
        if (_textSource is TextAndVersionSource concrete)
            concrete.SetValue(tav);
        return tav.Text;
    }

    public ValueTask<bool> TryGetTextAsync(out SourceText sourceText, CancellationToken cancellationToken)
    {
        if (_textSource.TryGetValue(out var tav))
        {
            sourceText = tav.Text;
            return ValueTask.FromResult(true);
        }

        sourceText = null!;
        return ValueTask.FromResult(false);
    }
}
