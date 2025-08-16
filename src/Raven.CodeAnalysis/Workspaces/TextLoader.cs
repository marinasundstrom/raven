using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a mechanism for asynchronously producing <see cref="SourceText"/>
/// instances with an associated <see cref="VersionStamp"/>.
/// </summary>
public abstract class TextLoader
{
    /// <summary>
    /// Asynchronously load the text and version information for a document.
    /// </summary>
    public abstract Task<TextAndVersion> LoadTextAndVersionAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Convenience helper that only returns the loaded <see cref="SourceText"/>.
    /// </summary>
    public async Task<SourceText> LoadTextAsync(CancellationToken cancellationToken = default)
        => (await LoadTextAndVersionAsync(cancellationToken).ConfigureAwait(false)).Text;

    /// <summary>
    /// Create a <see cref="TextLoader"/> that simply returns a fixed
    /// <see cref="TextAndVersion"/> instance.
    /// </summary>
    public static TextLoader From(TextAndVersion textAndVersion)
    {
        if (textAndVersion is null)
            throw new ArgumentNullException(nameof(textAndVersion));

        return new TextDocumentLoader(textAndVersion);
    }
}

