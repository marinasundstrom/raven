using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class AnalyzerInfrastructureTests
{
    private sealed class TodoAnalyzer : DiagnosticAnalyzer
    {
        public static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0001",
            title: "TODO found",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "TODO found",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(ctx =>
            {
                var text = ctx.SyntaxTree.GetText()?.ToString();
                if (text is not null && text.Contains("TODO"))
                    ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None));
            });
        }
    }

    [Fact]
    public void GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var initial = SourceText.From("\"unterminated");
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", initial);
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TodoAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics1 = workspace.GetDiagnostics(projectId);
        Assert.Contains(diagnostics1, d => d.Descriptor.Id == "RAV1010");
        Assert.DoesNotContain(diagnostics1, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);

        var updated = workspace.CurrentSolution.WithDocumentText(docId, SourceText.From("TODO \"unterminated"));
        workspace.TryApplyChanges(updated);

        var diagnostics2 = workspace.GetDiagnostics(projectId);
        Assert.Contains(diagnostics2, d => d.Descriptor.Id == "RAV1010");
        Assert.Contains(diagnostics2, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);
    }
}
