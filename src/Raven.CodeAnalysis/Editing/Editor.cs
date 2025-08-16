using Raven.CodeAnalysis.Text;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Editing;

/// <summary>
/// Provides a simple API for making textual edits to a <see cref="Document"/>.
/// Similar in spirit to Roslyn's <c>DocumentEditor</c>, this class allows callers
/// to build up changes and produce a new document with those changes applied.
/// </summary>
public sealed class Editor
{
    private readonly Document _originalDocument;
    private SourceText _text;

    private Editor(Document document, SourceText text)
    {
        _originalDocument = document;
        _text = text;
    }

    /// <summary>Creates a new <see cref="Editor"/> for the provided document.</summary>
    public static Editor Create(Document document)
    {
        if (document is null)
            throw new ArgumentNullException(nameof(document));

        var text = document.GetTextAsync().GetAwaiter().GetResult();
        return new Editor(document, text);
    }

    /// <summary>The document that edits are being applied to.</summary>
    public Document OriginalDocument => _originalDocument;

    /// <summary>Inserts <paramref name="newText"/> at the specified <paramref name="position"/>.</summary>
    public void Insert(int position, string newText)
    {
        if (newText is null)
            throw new ArgumentNullException(nameof(newText));

        _text = _text.WithChange(position, newText);
    }

    /// <summary>Replaces the text within <paramref name="span"/> with <paramref name="newText"/>.</summary>
    public void Replace(TextSpan span, string newText)
    {
        if (newText is null)
            throw new ArgumentNullException(nameof(newText));

        _text = _text.WithChange(new TextChange(span, newText));
    }

    /// <summary>Removes the text within the specified <paramref name="span"/>.</summary>
    public void Remove(TextSpan span)
    {
        _text = _text.WithChange(new TextChange(span, string.Empty));
    }

    /// <summary>Gets a new <see cref="Document"/> with all edits applied.</summary>
    public Document GetChangedDocument()
    {
        return _originalDocument.WithText(_text);
    }
}

