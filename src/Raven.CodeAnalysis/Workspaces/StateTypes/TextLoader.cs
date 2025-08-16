using System.Threading;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis;

public abstract class TextLoader
{
    public static TextLoader From(TextAndVersion textAndVersion)
    {
        if (textAndVersion == null)
        {
            throw new ArgumentNullException(nameof(textAndVersion));
        }
        return new TextDocumentLoader(textAndVersion);
    }

    public abstract Task<TextAndVersion> LoadTextAndVersionAsync(DocumentId documentId, CancellationToken cancellationToken);
}
