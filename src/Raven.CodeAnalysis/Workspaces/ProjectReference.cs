namespace Raven.CodeAnalysis;

public sealed class ProjectReference
{
    public ProjectId ProjectId { get; }

    public ProjectReference(ProjectId projectId)
    {
        ProjectId = projectId;
    }
}
