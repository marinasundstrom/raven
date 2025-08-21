using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class RavenWorkspaceTests
{
    [Fact]
    public void AddProject_ShouldNotIncludeFrameworkReferences()
    {
        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject("App");

        var compilation = workspace.GetCompilation(projectId);
        Assert.Single(compilation.References);
    }

    [Fact]
    public void AddProjectAndDocument_ShouldIncludeDocumentInProject()
    {
        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject("App");

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        var document = project.AddDocument(
            "test.rav",
            SourceText.From("System.Console.WriteLine(\"Hello\")\n"));
        workspace.TryApplyChanges(document.Project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        Assert.Contains(compilation.SyntaxTrees, t => t.FilePath == "test.rav");
    }
}
