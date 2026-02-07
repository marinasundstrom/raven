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

    [Fact]
    public void ApplyCodeFixes_PreferTargetTypedUnionCase_RewritesDeclaration()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
func Test() {
    val v = Option<int>.Some(0)
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new PreferTargetTypedUnionCaseAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var applyResult = workspace.ApplyCodeFixes(
            projectId,
            [new PreferTargetTypedUnionCaseCodeFixProvider()]);

        workspace.TryApplyChanges(applyResult.Solution);
        var updatedDoc = workspace.CurrentSolution.GetDocument(docId)!;
        var updatedText = updatedDoc.GetTextAsync().GetAwaiter().GetResult().ToString();

        Assert.Equal(1, applyResult.AppliedFixCount);
        Assert.Contains("val v: Option<int> = .Some(0)", updatedText, StringComparison.Ordinal);
    }

    [Fact]
    public void ApplyCodeFixes_PreferTargetTypedUnionCaseInTargetTypedContext_RewritesInvocationArgument()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
func Foo(x: Shape) {}

func Test() {
    Foo(Shape.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var applyResult = workspace.ApplyCodeFixes(
            projectId,
            [new PreferTargetTypedUnionCaseInTargetTypedContextCodeFixProvider()]);

        workspace.TryApplyChanges(applyResult.Solution);
        var updatedDoc = workspace.CurrentSolution.GetDocument(docId)!;
        var updatedText = updatedDoc.GetTextAsync().GetAwaiter().GetResult().ToString();

        Assert.Equal(1, applyResult.AppliedFixCount);
        Assert.Contains("Foo(.Circle(2))", updatedText, StringComparison.Ordinal);
    }
}
