namespace Raven.CodeAnalysis;

public interface IProjectSystemService
{
    bool CanOpenProject(string projectFilePath);

    IReadOnlyList<string> GetProjectReferencePaths(string projectFilePath);

    ProjectId OpenProject(Workspace workspace, string projectFilePath);

    void SaveProject(Project project, string filePath);
}
