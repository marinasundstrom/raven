namespace Raven.CodeAnalysis;

public class ProjectReference : IEquatable<ProjectReference>
{
    public ProjectReference(ProjectId projectId)
    {
        ProjectId = projectId;
    }

    public ProjectId ProjectId { get; set; }

    public bool Equals(ProjectReference? other)
    {
        return ProjectId == other?.ProjectId;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as ProjectReference);
    }

    public override int GetHashCode()
    {
        return ProjectId.GetHashCode();
    }

    public static bool operator ==(ProjectReference a, ProjectReference b)
    {
        return a.Equals(b);
    }

    public static bool operator !=(ProjectReference a, ProjectReference b)
    {
        return !a.Equals(b);
    }
}