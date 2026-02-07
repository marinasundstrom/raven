using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class CodeFixInfrastructureTests
{
    [Fact]
    public void ApplyCodeFixes_MissingReturnTypeAnnotation_AddsArrowTypeClause()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
func Test() {
    return 1
}
""";

        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var applyResult = workspace.ApplyCodeFixes(
            projectId,
            [new MissingReturnTypeAnnotationCodeFixProvider()]);

        workspace.TryApplyChanges(applyResult.Solution);
        var updatedDoc = workspace.CurrentSolution.GetDocument(docId)!;
        var updatedText = updatedDoc.GetTextAsync().GetAwaiter().GetResult().ToString();

        Assert.Equal(1, applyResult.AppliedFixCount);
        Assert.Contains("func Test() -> int {", updatedText, StringComparison.Ordinal);
    }
}
