namespace Raven.CodeAnalysis;

public sealed class TextDocumentLoader : TextLoader
{
    private TextAndVersion _textAndVersion;

    public TextDocumentLoader(TextAndVersion textAndVersion)
    {
        _textAndVersion = textAndVersion;
    }
}
