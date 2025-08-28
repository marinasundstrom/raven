using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class DiagnosticOptionsTests
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
    public void SpecificDiagnosticOptions_RemapsAnalyzerSeverity()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption(TodoAnalyzer.Descriptor.Id, ReportDiagnostic.Error);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From("TODO")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TodoAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId);
        var diagnostic = Assert.Single(diagnostics, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
    }

    [Fact]
    public void SpecificDiagnosticOptions_SuppressesCompilerDiagnostic()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption("RAV1010", ReportDiagnostic.Suppress);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From("\"unterminated")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV1010");
    }

    [Fact]
    public void RunAnalyzers_False_DisablesAnalyzerDiagnostics()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithRunAnalyzers(false);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From("TODO \"unterminated")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TodoAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId);
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV1010");
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);
    }

    [Fact]
    public void ReportSuppressedDiagnostics_True_ReportsSuppressed()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption("RAV1010", ReportDiagnostic.Suppress);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From("\"unterminated")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId, new CompilationWithAnalyzersOptions(reportSuppressedDiagnostics: true));
        var diagnostic = Assert.Single(diagnostics, d => d.Descriptor.Id == "RAV1010");
        Assert.True(diagnostic.IsSuppressed);
    }
}
