using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

/// <summary>
/// Default implementation of <see cref="ITextAndVersionSource"/> which simply
/// caches a single <see cref="TextAndVersion"/> instance once loaded.
/// </summary>
public sealed class TextAndVersionSource : ITextAndVersionSource
{
    private TextAndVersion? _textAndVersion;

    public TextAndVersionSource(TextLoader loader, TextAndVersion? initial = null)
    {
        TextLoader = loader ?? throw new ArgumentNullException(nameof(loader));
        _textAndVersion = initial;
    }

    public TextLoader TextLoader { get; }

    public bool TryGetValue([NotNullWhen(true)] out TextAndVersion? textAndVersion)
    {
        textAndVersion = _textAndVersion;
        return textAndVersion is not null;
    }

    /// <summary>
    /// Store the specified <see cref="TextAndVersion"/> so subsequent
    /// requests via <see cref="TryGetValue"/> succeed without reloading.
    /// </summary>
    public void SetValue(TextAndVersion textAndVersion)
        => _textAndVersion = textAndVersion ?? throw new ArgumentNullException(nameof(textAndVersion));
}
