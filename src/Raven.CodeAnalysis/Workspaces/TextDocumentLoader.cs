using System.Threading;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis;

public sealed class TextDocumentLoader : TextLoader
{
    private readonly TextAndVersion _textAndVersion;

    public TextDocumentLoader(TextAndVersion textAndVersion)
    {
        _textAndVersion = textAndVersion;
    }

    public override Task<TextAndVersion> LoadTextAndVersionAsync(DocumentId documentId, CancellationToken cancellationToken)
    {
        return Task.FromResult(_textAndVersion);
    }
}
