using System.Threading;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis;

/// <summary>
/// Simple <see cref="TextLoader"/> that always returns a pre-supplied
/// <see cref="TextAndVersion"/> instance.
/// </summary>
public sealed class TextDocumentLoader : TextLoader
{
    private readonly TextAndVersion _textAndVersion;

    public TextDocumentLoader(TextAndVersion textAndVersion)
    {
        _textAndVersion = textAndVersion ?? throw new ArgumentNullException(nameof(textAndVersion));
    }

    public override Task<TextAndVersion> LoadTextAndVersionAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(_textAndVersion);
}
