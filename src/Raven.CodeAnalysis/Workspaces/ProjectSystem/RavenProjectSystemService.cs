using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

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

    public ProjectId OpenProject(Workspace workspace, string projectFilePath)
    {
        if (workspace is not RavenWorkspace raven)
            throw new NotSupportedException("Project persistence requires a RavenWorkspace.");

        var projInfo = ProjectFile.Load(projectFilePath);
        var projectId = raven.AddProject(projInfo.Info.Name, projectFilePath, projInfo.Info.AssemblyName, projInfo.Info.CompilationOptions);

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
            projInfo.Configuration);

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
        string? configuration)
    {
        if (string.IsNullOrWhiteSpace(targetFramework))
            return solution;

        var project = solution.GetProject(projectId);
        if (project is null || ContainsAssemblyTargetFrameworkAttribute(project))
            return solution;

        if (!TargetFrameworkMoniker.TryParse(targetFramework, out var tfm) || tfm is null)
            return solution;

        var frameworkString = tfm.ToFrameworkString();
        var generatedSource = string.Join(
            Environment.NewLine,
            "import System.Runtime.Versioning.*",
            string.Empty,
            $"[assembly: TargetFramework(\"{EscapeRavenString(frameworkString)}\")]",
            string.Empty);

        var projectDirectory = Path.GetDirectoryName(projectFilePath) ?? Environment.CurrentDirectory;
        var generatedDirectory = _conventions.GetGeneratedSourceDirectory(projectDirectory, configuration);
        Directory.CreateDirectory(generatedDirectory);

        var generatedName = _conventions.GetTargetFrameworkAttributeFileName(project.Name);
        var generatedPath = Path.Combine(generatedDirectory, generatedName);
        File.WriteAllText(generatedPath, generatedSource);

        var generatedId = DocumentId.CreateNew(projectId);
        return solution.AddDocument(generatedId, generatedName, SourceText.From(generatedSource), generatedPath);
    }

    private static bool ContainsAssemblyTargetFrameworkAttribute(Project project)
    {
        foreach (var document in project.Documents)
        {
            if (!RavenFileExtensions.HasRavenExtension(document.Name) &&
                (document.FilePath is null || !RavenFileExtensions.HasRavenExtension(document.FilePath)))
            {
                continue;
            }

            var tree = SyntaxTree.ParseText(document.Text, path: document.FilePath ?? document.Name);
            if (tree.GetRoot() is not CompilationUnitSyntax compilationUnit)
                continue;

            foreach (var attributeList in compilationUnit.AttributeLists)
            {
                if (!string.Equals(attributeList.Target?.Identifier.ValueText, "assembly", StringComparison.OrdinalIgnoreCase))
                    continue;

                foreach (var attribute in attributeList.Attributes)
                {
                    var name = attribute.Name.ToString();
                    if (name.Equals("TargetFramework", StringComparison.OrdinalIgnoreCase) ||
                        name.Equals("TargetFrameworkAttribute", StringComparison.OrdinalIgnoreCase) ||
                        name.EndsWith(".TargetFramework", StringComparison.OrdinalIgnoreCase) ||
                        name.EndsWith(".TargetFrameworkAttribute", StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    private static string EscapeRavenString(string value)
        => value.Replace("\\", "\\\\", StringComparison.Ordinal).Replace("\"", "\\\"", StringComparison.Ordinal);
}
