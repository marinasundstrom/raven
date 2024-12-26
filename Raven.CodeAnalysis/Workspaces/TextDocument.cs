using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public class TextDocument
{
    internal SourceText _sourceText;

    public string FilePath { get; set; }

    public IEnumerable<string> Folders { get; set; }

    public DocumentId Id { get; set; }

    public string Name { get; set; }

    public Project Project { get; set; }

    internal async Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
    {
        if (_sourceText is not null)
        {
            return _sourceText;
        }

        // Load text from file or memory, depending on the setup
        if (!string.IsNullOrEmpty(FilePath))
        {
            var text = await File.ReadAllTextAsync(FilePath, cancellationToken);
            return SourceText.From(text);
        }

        throw new InvalidOperationException("No text source available.");
    }

    public Task<VersionStamp> GetTextVersionAsync(CancellationToken cancellationToken)
    {
        return null;
    }
}
