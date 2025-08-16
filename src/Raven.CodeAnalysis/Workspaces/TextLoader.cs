using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Base abstraction for loading document text and version information.
/// </summary>
public abstract class TextLoader
{
    public abstract Task<TextAndVersion> LoadTextAndVersionAsync(DocumentId documentId, CancellationToken cancellationToken);

    /// <summary>
    /// Creates a loader that simply returns the provided text immediately.
    /// </summary>
    public static TextLoader From(SourceText text) => new SourceTextLoader(text);

    private sealed class SourceTextLoader : TextLoader
    {
        private readonly SourceText _text;

        public SourceTextLoader(SourceText text) => _text = text;

        public override Task<TextAndVersion> LoadTextAndVersionAsync(DocumentId documentId, CancellationToken cancellationToken)
            => Task.FromResult(new TextAndVersion(_text, VersionStamp.Create()));
    }
}

