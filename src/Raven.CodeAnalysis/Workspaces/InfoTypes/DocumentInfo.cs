using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>Immutable metadata about a document (file path, name, etc.).</summary>
public sealed class DocumentInfo
{
    public DocumentInfo(DocumentAttributes attributes, TextLoader textLoader)
    {
        Attributes = attributes;
        TextLoader = textLoader;
    }

    public DocumentAttributes Attributes { get; }

    public DocumentId Id => Attributes.Id;
    public string Name => Attributes.Name;
    public string? FilePath => Attributes.FilePath;
    public TextLoader TextLoader { get; }

    /// <summary>Factory helper for convenience.</summary>
    public static DocumentInfo Create(DocumentId id, string name, SourceText text, string? filePath = null) =>
        new(new DocumentAttributes(id, name, filePath), TextLoader.From(text));

    public DocumentInfo WithText(SourceText newText) => new(Attributes, TextLoader.From(newText));

    public DocumentInfo WithName(string newName) => new(Attributes.WithName(newName), TextLoader);

    public DocumentInfo WithFilePath(string? newPath) => new(Attributes.WithFilePath(newPath), TextLoader);

    /// <summary>Record that actually stores the data. Immutable for structural sharing.</summary>
    public sealed record DocumentAttributes(
        DocumentId Id,
        string Name,
        string? FilePath)
    {
        public DocumentAttributes WithName(string name) => name == Name ? this : this with { Name = name };
        public DocumentAttributes WithFilePath(string? path) => path == FilePath ? this : this with { FilePath = path };
    }
}

