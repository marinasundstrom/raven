using System.Diagnostics.CodeAnalysis;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class TextAndVersionSource : ITextAndVersionSource
{
    private TextAndVersion? _textAndVersion;

    public TextAndVersionSource(TextLoader textLoader, TextAndVersion? textAndVersion = null)
    {
        TextLoader = textLoader;
        _textAndVersion = textAndVersion;
    }

    public TextLoader TextLoader { get; }

    public bool TryGetValue([NotNullWhen(true)] out TextAndVersion? textAndVersion)
    {
        textAndVersion = _textAndVersion;
        return textAndVersion is not null;
    }

    public async Task<SourceText> GetTextAsync(DocumentId documentId, CancellationToken cancellationToken)
    {
        if (_textAndVersion is null)
        {
            _textAndVersion = await TextLoader.LoadTextAndVersionAsync(documentId, cancellationToken);
        }

        return _textAndVersion.Text;
    }
}
