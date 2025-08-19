using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>Immutable metadata about a document (file path, name, etc.).</summary>
public sealed class DocumentInfo
{
    public DocumentInfo(DocumentAttributes attributes) => Attributes = attributes;

    public DocumentAttributes Attributes { get; }

    public DocumentId Id => Attributes.Id;
    public string Name => Attributes.Name;
    public SourceText Text => Attributes.Text;
    public VersionStamp Version => Attributes.Version;
    public string? FilePath => Attributes.FilePath;

    /// <summary>Factory helper for convenience.</summary>
    public static DocumentInfo Create(DocumentId id, string name, SourceText text, string? filePath = null) =>
        new(new DocumentAttributes(id, name, text, VersionStamp.Create(), filePath));

    public DocumentInfo WithText(SourceText newText) => new(Attributes.WithText(newText));

    public DocumentInfo WithName(string newName) => new(Attributes.WithName(newName));

    public DocumentInfo WithFilePath(string? newPath) => new(Attributes.WithFilePath(newPath));

    /// <summary>Record that actually stores the data. Immutable for structural sharing.</summary>
    public sealed record DocumentAttributes(
        DocumentId Id,
        string Name,
        SourceText Text,
        VersionStamp Version,
        string? FilePath)
    {
        public DocumentAttributes WithText(SourceText text) =>
            text == Text ? this : this with { Text = text, Version = Version.GetNewerVersion() };
        public DocumentAttributes WithName(string name) =>
            name == Name ? this : this with { Name = name, Version = Version.GetNewerVersion() };
        public DocumentAttributes WithFilePath(string? path) =>
            path == FilePath ? this : this with { FilePath = path, Version = Version.GetNewerVersion() };
    }
}
