using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

public readonly struct DocumentId : IEquatable<DocumentId>
{
    private DocumentId(ProjectId projectId, string id)
    {
        ProjectId = projectId;
        Id = id;
    }

    public ProjectId ProjectId { get; }

    public string Id { get; }

    public static DocumentId CreateNewId(ProjectId projectId, string id) => new DocumentId(projectId, id);

    public static DocumentId CreateNewId(ProjectId projectId) => new DocumentId(projectId, Guid.NewGuid().ToString());

    public bool Equals(DocumentId other)
    {
        return ProjectId == other.ProjectId && Id == other.Id;
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is not DocumentId documentId)
            return false;

        return Equals(documentId);
    }

    public override int GetHashCode()
    {
        return ProjectId.GetHashCode() ^ Id.GetHashCode();
    }

    public static bool operator ==(DocumentId a, DocumentId b)
    {
        return a.Equals(b);
    }

    public static bool operator !=(DocumentId a, DocumentId b)
    {
        return !a.Equals(b);
    }

    public override string ToString()
    {
        return Id;
    }
}
