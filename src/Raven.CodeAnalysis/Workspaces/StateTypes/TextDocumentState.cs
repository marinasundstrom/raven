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

    public DocumentId Id => _attributes.Id;
    public string Name => _attributes.Name;
    public SourceText Text => _attributes.Text;
    public string? FilePath => _attributes.FilePath;
    public ITextAndVersionSource TextAndVersionSource => _textSource;

    public ValueTask<SourceText> GetTextAsync(CancellationToken cancellationToken)
    {
        return ValueTask.FromResult(Text);
    }

    public ValueTask<bool> TryGetTextAsync(out SourceText sourceText, CancellationToken cancellationToken)
    {
        sourceText = Text;
        return ValueTask.FromResult(true);
    }
}
