using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class AnalyzerInfrastructureTests
{
    private sealed class ReservedPrefixAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "RAV9999",
            title: "Reserved prefix",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Reserved prefix",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(ctx =>
            {
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None));
            });
        }
    }

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

    private sealed class NodeKindAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0002",
            title: "Node match",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Node kind match",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(
                ctx => ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, ctx.Node.GetLocation())),
                SyntaxKind.MethodDeclaration);
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
        Assert.Contains(diagnostics2, d => d.Descriptor.Id == "RAV0103");
        Assert.Contains(diagnostics2, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);
    }

    [Fact]
    public void GetDiagnostics_ExternalAnalyzerWithReservedRavPrefix_Throws()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From("TODO"));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new ReservedPrefixAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        Should.Throw<InvalidOperationException>(() => workspace.GetDiagnostics(projectId))
            .Message.ShouldContain("reserved 'RAV' prefix");
    }

    [Fact]
    public void GetDiagnostics_SyntaxNodeAction_OnlyRunsForRegisteredKinds()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
class C {
    public M() -> unit { }
}

func F() -> unit { }
""";
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new NodeKindAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId)
            .Where(d => d.Descriptor.Id == "AN0002")
            .ToList();

        diagnostics.Count.ShouldBe(1);
    }
}
