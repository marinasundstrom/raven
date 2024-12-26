namespace Raven.CodeAnalysis;

public class DocumentInfo
{
    private readonly DocumentAttributes _attributes;

    public DocumentInfo(DocumentAttributes attributes)
    {
        _attributes = attributes;
    }

    internal DocumentInfo With(
        DocumentId id,
        string name,
        IReadOnlyList<string> folders,
        SourceCodeKind sourceCodeKind,
        string? filePath,
        bool isGenerated,
        bool designTimeOnly
    )
    {
        return new DocumentInfo(_attributes.With(
            id,
            name,
            folders,
            sourceCodeKind,
            filePath,
            isGenerated,
            designTimeOnly
        ));
    }
}
