using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class RavenWorkspaceTests
{
    [Fact]
    public void AddProject_ShouldIncludeFrameworkReferences()
    {
        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject("App");

        var compilation = workspace.GetCompilation(projectId);
        Assert.NotEmpty(compilation.References);
    }

    [Fact]
    public void AddProjectAndDocument_ShouldIncludeDocumentInProject()
    {
        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject("App");

        workspace.CurrentSolution.AddDocument(
                DocumentId.CreateNew(projectId),
                "test.rav",
                SourceText.From("System.Console.WriteLine(\"Hello\")\n"));

        var compilation = workspace.GetCompilation(projectId);
        Assert.NotEmpty(compilation.References);
    }
}
