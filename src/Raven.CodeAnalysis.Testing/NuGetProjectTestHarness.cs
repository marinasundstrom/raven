using System.Text;
using System.Xml.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Testing;

public sealed class NuGetProjectTestHarness : IDisposable
{
    private readonly string _rootDirectory;
    private bool _disposed;

    private NuGetProjectTestHarness(
        string rootDirectory,
        string projectFilePath,
        RavenWorkspace workspace,
        ProjectId projectId)
    {
        _rootDirectory = rootDirectory;
        ProjectFilePath = projectFilePath;
        Workspace = workspace;
        ProjectId = projectId;
    }

    public string ProjectFilePath { get; }

    public RavenWorkspace Workspace { get; }

    public ProjectId ProjectId { get; }

    public Project Project => Workspace.CurrentSolution.GetProject(ProjectId)
        ?? throw new InvalidOperationException("Project not found in workspace.");

    public Compilation GetCompilation() => Workspace.GetCompilation(ProjectId);

    public SyntaxTree GetSyntaxTree(string relativePath)
    {
        var fullPath = Path.Combine(_rootDirectory, relativePath);
        return GetCompilation().SyntaxTrees.First(tree =>
            string.Equals(Path.GetFullPath(tree.FilePath ?? string.Empty), Path.GetFullPath(fullPath), StringComparison.OrdinalIgnoreCase));
    }

    public void Dispose()
    {
        if (_disposed)
            return;

        _disposed = true;

        try
        {
            Directory.Delete(_rootDirectory, recursive: true);
        }
        catch
        {
            // Best-effort cleanup for temporary test projects.
        }
    }

    public static NuGetProjectTestHarness Create(
        string source,
        IEnumerable<(string Id, string Version)> packageReferences,
        CompilationOptions? compilationOptions = null,
        string? targetFramework = null,
        string projectName = "TestProject",
        string documentPath = "src/main.rav")
    {
        ArgumentNullException.ThrowIfNull(source);
        ArgumentNullException.ThrowIfNull(packageReferences);

        var rootDirectory = Path.Combine(
            Path.GetTempPath(),
            $"raven-nuget-test-{Guid.NewGuid():N}");
        Directory.CreateDirectory(rootDirectory);

        var fullDocumentPath = Path.Combine(rootDirectory, documentPath);
        Directory.CreateDirectory(Path.GetDirectoryName(fullDocumentPath)!);
        File.WriteAllText(fullDocumentPath, source, Encoding.UTF8);

        var projectFilePath = Path.Combine(rootDirectory, $"{projectName}.ravenproj");
        var projectElement = new XElement(
            "Project",
            new XAttribute("Name", projectName),
            new XAttribute("TargetFramework", targetFramework ?? TestTargetFramework.Default),
            new XAttribute("Output", projectName),
            new XAttribute("OutputKind", (compilationOptions ?? new CompilationOptions(OutputKind.ConsoleApplication)).OutputKind),
            new XElement("Document", new XAttribute("Path", documentPath)));

        foreach (var packageReference in packageReferences)
        {
            projectElement.Add(
                new XElement(
                    "PackageReference",
                    new XAttribute("Include", packageReference.Id),
                    new XAttribute("Version", packageReference.Version)));
        }

        File.WriteAllText(projectFilePath, new XDocument(projectElement).ToString(), Encoding.UTF8);

        var workspace = RavenWorkspace.Create(targetFramework: targetFramework ?? TestTargetFramework.Default);
        var projectId = workspace.OpenProject(projectFilePath);
        var project = workspace.CurrentSolution.GetProject(projectId)
            ?? throw new InvalidOperationException("Temporary project failed to load.");

        if (compilationOptions is not null)
        {
            project = project.WithCompilationOptions(compilationOptions);
            workspace.TryApplyChanges(project.Solution);
        }

        return new NuGetProjectTestHarness(rootDirectory, projectFilePath, workspace, projectId);
    }
}
