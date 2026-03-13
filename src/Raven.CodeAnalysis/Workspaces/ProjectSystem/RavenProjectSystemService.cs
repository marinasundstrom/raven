using System;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class RavenProjectSystemService : IProjectSystemService
{
    private readonly RavenProjectConventions _conventions;

    public RavenProjectSystemService()
        : this(RavenProjectConventions.Default)
    {
    }

    public RavenProjectSystemService(RavenProjectConventions conventions)
    {
        _conventions = conventions ?? throw new ArgumentNullException(nameof(conventions));
    }

    public bool CanOpenProject(string projectFilePath)
        => string.Equals(Path.GetExtension(projectFilePath), RavenFileExtensions.LegacyProject, StringComparison.OrdinalIgnoreCase);

    public IReadOnlyList<string> GetProjectReferencePaths(string projectFilePath)
        => ProjectFile.Load(projectFilePath).ProjectReferences;

    public ProjectId OpenProject(Workspace workspace, string projectFilePath)
    {
        if (workspace is not RavenWorkspace raven)
            throw new NotSupportedException("Project persistence requires a RavenWorkspace.");

        var projInfo = ProjectFile.Load(projectFilePath);
        var projectId = raven.AddProject(
            projInfo.Info.Name,
            projectFilePath,
            projInfo.Info.AssemblyName,
            projInfo.Info.CompilationOptions,
            projInfo.Info.TargetFramework);

        var solution = workspace.CurrentSolution;
        foreach (var doc in projInfo.Info.Documents)
        {
            var docId = DocumentId.CreateNew(projectId);
            solution = solution.AddDocument(docId, doc.Name, doc.Text, doc.FilePath);
        }

        solution = AddGeneratedTargetFrameworkAttributeDocumentIfNeeded(
            solution,
            projectId,
            projectFilePath,
            projInfo.Info.TargetFramework,
            projInfo.Configuration,
            projInfo.Info.Name);

        var tfm = projInfo.Info.TargetFramework ?? raven.DefaultTargetFramework;
        foreach (var reference in raven.GetFrameworkReferences(tfm))
            solution = solution.AddMetadataReference(projectId, reference);

        foreach (var metadataReferencePath in projInfo.MetadataReferences)
            solution = solution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(metadataReferencePath));

        var packageReferences = NuGetPackageResolver.ResolveReferences(
            projectFilePath,
            tfm,
            projInfo.PackageReferences,
            projInfo.FrameworkReferences);

        foreach (var packageReference in packageReferences)
            solution = solution.AddMetadataReference(projectId, packageReference);

        foreach (var refPath in projInfo.ProjectReferences)
        {
            var full = Path.IsPathRooted(refPath)
                ? refPath
                : Path.GetFullPath(Path.Combine(Path.GetDirectoryName(projectFilePath)!, refPath));
            var refProj = raven.CurrentSolution.Projects.FirstOrDefault(
                p => string.Equals(p.FilePath, full, StringComparison.OrdinalIgnoreCase));
            if (refProj is not null)
                solution = solution.AddProjectReference(projectId, new ProjectReference(refProj.Id));
        }

        workspace.TryApplyChanges(solution);
        return projectId;
    }

    public void SaveProject(Project project, string filePath)
        => ProjectFile.Save(project, filePath);

    private Solution AddGeneratedTargetFrameworkAttributeDocumentIfNeeded(
        Solution solution,
        ProjectId projectId,
        string projectFilePath,
        string? targetFramework,
        string? configuration,
        string projectName)
    {
        var projectDirectory = Path.GetDirectoryName(projectFilePath) ?? Environment.CurrentDirectory;
        var generatedDirectory = _conventions.GetGeneratedSourceDirectory(projectDirectory, configuration);
        var generatedName = _conventions.GetTargetFrameworkAttributeFileName(projectName);
        return ProjectSystemGeneratedDocumentHelper.AddGeneratedTargetFrameworkAttributeDocumentIfNeeded(
            solution,
            projectId,
            generatedDirectory,
            generatedName,
            targetFramework);
    }
}
