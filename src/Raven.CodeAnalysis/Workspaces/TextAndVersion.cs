using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class TextAndVersion
{
    public TextAndVersion(SourceText text, VersionStamp version, string? filePath = null)
    {
        Text = text ?? throw new ArgumentNullException(nameof(text));
        Version = version;
        FilePath = filePath;
    }

    public SourceText Text { get; }

    public VersionStamp Version { get; }

    public string? FilePath { get; }
}
