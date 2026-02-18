using System;
using System.IO;
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
        Assert.Contains(diagnostics, d => d.Descriptor.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id);
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

    [Fact]
    public void SpecificDiagnosticOptions_RemapsNonNullDeclarationsSeverity()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption(NonNullDeclarationsAnalyzer.DiagnosticId, ReportDiagnostic.Error);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(
            """
func Test() {
    var value: int? = null
}
""")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new NonNullDeclarationsAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId);
        var diagnostic = Assert.Single(diagnostics, d => d.Descriptor.Id == NonNullDeclarationsAnalyzer.DiagnosticId);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
    }

    [Fact]
    public void OpenProject_EditorConfigSuppressesConfiguredAnalyzerDiagnostics()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);

        try
        {
            var projectPath = Path.Combine(dir, "App.ravenproj");
            var sourcePath = Path.Combine(dir, "main.rav");
            var editorConfigPath = Path.Combine(dir, ".editorconfig");

            File.WriteAllText(sourcePath,
                """
import System.Linq.*

func Main() {
    var maybe: int? = null
    val arr = [1, 2, 3]
    val x = arr.FirstOrDefault()
    throw Exception("boom")
}
""");

            File.WriteAllText(projectPath,
                """
<Project Name="App" TargetFramework="net9.0" Output="App">
  <Document Path="main.rav" />
</Project>
""");

            File.WriteAllText(editorConfigPath,
                """
root = true

[*.rav]
dotnet_diagnostic.RAV9012.severity = none
dotnet_diagnostic.RAV9013.severity = none
dotnet_diagnostic.RAV9014.severity = none
""");

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            project = project
                .AddAnalyzerReference(new AnalyzerReference(new NonNullDeclarationsAnalyzer()))
                .AddAnalyzerReference(new AnalyzerReference(new ThrowStatementUseResultAnalyzer()))
                .AddAnalyzerReference(new AnalyzerReference(new PreferDuLinqExtensionsAnalyzer()));

            foreach (var reference in TestMetadataReferences.Default)
                project = project.AddMetadataReference(reference);

            workspace.TryApplyChanges(project.Solution);

            var diagnostics = workspace.GetDiagnostics(projectId);
            Assert.DoesNotContain(diagnostics, d => d.Id == NonNullDeclarationsAnalyzer.DiagnosticId);
            Assert.DoesNotContain(diagnostics, d => d.Id == ThrowStatementUseResultAnalyzer.DiagnosticId);
            Assert.DoesNotContain(diagnostics, d => d.Id == PreferDuLinqExtensionsAnalyzer.DiagnosticId);
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    [Fact]
    public void OpenProject_EditorConfigGlobalAnalyzerSeveritySuppressesAnalyzerDiagnostics()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);

        try
        {
            var projectPath = Path.Combine(dir, "App.ravenproj");
            var sourcePath = Path.Combine(dir, "main.rav");
            var editorConfigPath = Path.Combine(dir, ".editorconfig");

            File.WriteAllText(sourcePath, "TODO");
            File.WriteAllText(projectPath,
                """
<Project Name="App" TargetFramework="net9.0" Output="App">
  <Document Path="main.rav" />
</Project>
""");
            File.WriteAllText(editorConfigPath,
                """
root = true

[*.rav]
dotnet_analyzer_diagnostic.severity = none
""");

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            project = project.AddAnalyzerReference(new AnalyzerReference(new TodoAnalyzer()));
            foreach (var reference in TestMetadataReferences.Default)
                project = project.AddMetadataReference(reference);
            workspace.TryApplyChanges(project.Solution);

            var diagnostics = workspace.GetDiagnostics(projectId);
            Assert.DoesNotContain(diagnostics, d => d.Id == TodoAnalyzer.Descriptor.Id);
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }
}
