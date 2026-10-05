namespace Raven.CodeAnalysis;

public interface IProjectSystemService
{
    bool CanOpenProject(string projectFilePath);

    IReadOnlyList<string> GetProjectReferencePaths(string projectFilePath);

    ProjectId OpenProject(Workspace workspace, string projectFilePath);

    /// <summary>Explicit artifact/configuration files whose replacement requires project reload.</summary>
    IReadOnlyList<string> GetMetadataInputPaths(string projectFilePath) => [];

    void SaveProject(Project project, string filePath);
}
