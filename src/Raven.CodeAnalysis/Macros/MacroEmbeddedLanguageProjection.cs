using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// A position-preserving embedded-language view of a token-tree macro body.
/// </summary>
public sealed class MacroEmbeddedLanguageProjection
{
    internal MacroEmbeddedLanguageProjection(string languageId, string text, TextSpan span)
    {
        LanguageId = languageId;
        Text = text;
        Span = span;
    }

    /// <summary>
    /// Gets the editor language identifier, such as <c>html</c>.
    /// </summary>
    public string LanguageId { get; }

    /// <summary>
    /// Gets the projected text. Its offsets and line breaks match <see cref="Span"/>.
    /// </summary>
    public string Text { get; }

    /// <summary>
    /// Gets the authored span represented by <see cref="Text"/>.
    /// </summary>
    public TextSpan Span { get; }
}
