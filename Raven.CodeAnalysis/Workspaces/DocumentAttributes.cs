namespace Raven.CodeAnalysis;

public class DocumentAttributes
{
    public DocumentAttributes(
        DocumentId id,
        string name,
        IReadOnlyList<string> folders,
        SourceCodeKind sourceCodeKind,
        string? filePath,
        bool isGenerated,
        bool designTimeOnly)
    {
        Id = id;
        Name = name;
        Folders = folders;
        SourceCodeKind = sourceCodeKind;
        FilePath = filePath;
        IsGenerated = isGenerated;
        DesignTimeOnly = designTimeOnly;
    }

    public DocumentId Id { get; }
    public string Name { get; }
    public IReadOnlyList<string> Folders { get; }
    public SourceCodeKind SourceCodeKind { get; }
    public string? FilePath { get; }
    public bool IsGenerated { get; }
    public bool DesignTimeOnly { get; }

    internal DocumentAttributes With(DocumentId id, string name, IReadOnlyList<string> folders, SourceCodeKind sourceCodeKind, string? filePath, bool isGenerated, bool designTimeOnly)
    {
        return new DocumentAttributes(id,
            name,
            folders,
            sourceCodeKind,
            filePath,
            isGenerated,
            designTimeOnly)
    }
}
