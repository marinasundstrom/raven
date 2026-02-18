namespace Raven.CodeAnalysis;

public interface IProjectSystemService
{
    ProjectId OpenProject(Workspace workspace, string projectFilePath);

    void SaveProject(Project project, string filePath);
}
