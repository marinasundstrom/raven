using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class TextAndVersion
{
    public TextAndVersion(SourceText text, VersionStamp version, string? filePath = null)
    {
        Text = text;
        Version = version;
        FilePath = filePath;
    }

    public SourceText Text { get; set; }

    public VersionStamp Version { get; set; }

    public string? FilePath { get; set; }
}
