using Raven.CodeAnalysis;
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
}
