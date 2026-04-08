using System.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class SingleFileWorkspaceCompilationTests
{
    [Fact]
    public void WorkspaceCompilation_SingleTopLevelInterface_DoesNotDuplicateDeclaration()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "test.rav",
            SourceText.From(
                """
                interface IError { }
                """),
            "/tmp/test.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Single(compilation.SyntaxTrees);
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.TypeAlreadyDefined.Id);
    }
}
