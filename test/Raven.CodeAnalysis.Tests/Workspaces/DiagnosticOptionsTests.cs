using System;
using System.Collections.Immutable;
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

    private sealed class DisabledByDefaultAnalyzer : DiagnosticAnalyzer
    {
        public static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0002",
            title: "Disabled by default",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Disabled by default",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info,
            isEnabledByDefault: false);

        public override void Initialize(AnalysisContext context)
            => context.RegisterSyntaxTreeAction(ctx =>
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None)));
    }

    [Fact]
    public void DisabledByDefaultAnalyzer_IsSuppressedWithoutExplicitSeverity()
    {
        var diagnostics = GetDisabledByDefaultAnalyzerDiagnostics(new CompilationOptions(OutputKind.ConsoleApplication));

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == DisabledByDefaultAnalyzer.Descriptor.Id);
    }

    [Fact]
    public void DisabledByDefaultAnalyzer_IsEnabledByExplicitSeverity()
    {
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption(DisabledByDefaultAnalyzer.Descriptor.Id, ReportDiagnostic.Warn);

        var diagnostics = GetDisabledByDefaultAnalyzerDiagnostics(options);
        var diagnostic = Assert.Single(diagnostics, diagnostic => diagnostic.Id == DisabledByDefaultAnalyzer.Descriptor.Id);
        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
    }

    [Fact]
    public void SpecificDiagnosticOptions_RemapsAnalyzerSeverity()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption(TodoAnalyzer.Descriptor.Id, ReportDiagnostic.Error);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("TODO")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TodoAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId);
        var diagnostic = Assert.Single(diagnostics, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
    }

    private static ImmutableArray<Diagnostic> GetDisabledByDefaultAnalyzerDiagnostics(CompilationOptions options)
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var documentId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From("val x = 1")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new DisabledByDefaultAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        return workspace.GetDiagnostics(projectId);
    }

    [Fact]
    public void SpecificDiagnosticOptions_SuppressesCompilerDiagnostic()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption("RAV1010", ReportDiagnostic.Suppress);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("\"unterminated")));

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
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("TODO \"unterminated")));

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
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("\"unterminated")));

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
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(
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
    public void SpecificDiagnosticOptions_SuppressesUnusedExpressionResult()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithSpecificDiagnosticOption(UnusedExpressionResultAnalyzer.DiagnosticId, ReportDiagnostic.Suppress);
        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var docId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(
            """
func Test() {
    GetValue()
}

func GetValue() -> int {
    42
}
""")));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new UnusedExpressionResultAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == UnusedExpressionResultAnalyzer.DiagnosticId);
    }

    [Fact]
    public void OpenProject_EditorConfigSuppressesConfiguredAnalyzerDiagnostics()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);

        try
        {
            var projectPath = Path.Combine(dir, "App.rvnproj");
            var sourcePath = Path.Combine(dir, "main.rvn");
            var editorConfigPath = Path.Combine(dir, ".editorconfig");

            File.WriteAllText(sourcePath,
                """
import System.Linq.*

func Main() {
    var maybe: int? = null
    let arr = [1, 2, 3]
    let x = arr.FirstOrDefault()
    throw Exception("boom")
}
""");

            File.WriteAllText(projectPath,
                """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
  </PropertyGroup>
</Project>
""");

            File.WriteAllText(editorConfigPath,
                """
root = true

[*.rvn]
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
            var projectPath = Path.Combine(dir, "App.rvnproj");
            var sourcePath = Path.Combine(dir, "main.rvn");
            var editorConfigPath = Path.Combine(dir, ".editorconfig");

            File.WriteAllText(sourcePath, "TODO");
            File.WriteAllText(projectPath,
                """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
  </PropertyGroup>
</Project>
""");
            File.WriteAllText(editorConfigPath,
                """
root = true

[*.rvn]
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
