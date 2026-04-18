using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class DocumentScopedDiagnosticsTests
{
    [Fact]
    public void CompilationGetDiagnostics_SyntaxTree_ReturnsOnlyDiagnosticsForRequestedTree()
    {
        var treeA = SyntaxTree.ParseText(
            """
            func Main() -> () {
                val value: MissingType = 42
            }
            """,
            path: "/tmp/a.rav");
        var treeB = SyntaxTree.ParseText(
            """
            func Helper() -> () {
                val value: OtherMissingType = 42
            }
            """,
            path: "/tmp/b.rav");

        var compilation = Compilation.Create(
            "test",
            [treeA, treeB],
            [.. TestMetadataReferences.Default],
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics(treeA);

        diagnostics.ShouldNotBeEmpty();
        diagnostics.ShouldAllBe(diagnostic =>
            string.Equals(diagnostic.Location.GetLineSpan().Path, "/tmp/a.rav", StringComparison.OrdinalIgnoreCase));
        diagnostics.ShouldNotContain(diagnostic =>
            string.Equals(diagnostic.Location.GetLineSpan().Path, "/tmp/b.rav", StringComparison.OrdinalIgnoreCase));
    }

    [Fact]
    public void WorkspaceGetDocumentDiagnostics_ReturnsOnlyDiagnosticsForRequestedDocument()
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
            "a.rav",
            SourceText.From(
                """
                func Main() -> () {
                    val value: MissingType = 42
                }
                """),
            "/tmp/a.rav").Project;

        project = project.AddDocument(
            "b.rav",
            SourceText.From(
                """
                func Helper() -> () {
                    val value: OtherMissingType = 42
                }
                """),
            "/tmp/b.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var documentA = workspace.CurrentSolution.Projects.Single().Documents.Single(document => document.FilePath == "/tmp/a.rav");
        var diagnostics = workspace.GetDocumentDiagnostics(projectId, documentA.Id);

        diagnostics.ShouldNotBeEmpty();
        diagnostics.ShouldAllBe(diagnostic =>
            string.Equals(diagnostic.Location.GetLineSpan().Path, "/tmp/a.rav", StringComparison.OrdinalIgnoreCase));
        diagnostics.ShouldNotContain(diagnostic =>
            string.Equals(diagnostic.Location.GetLineSpan().Path, "/tmp/b.rav", StringComparison.OrdinalIgnoreCase));
    }

    [Fact]
    public void WorkspaceCreateAnalysisCompilation_CachesPerProjectVersion()
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
            "a.rav",
            SourceText.From(
                """
                func Main() -> () {
                    val value: MissingType = 42
                }
                """),
            "/tmp/a.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.CreateAnalysisCompilation(projectId);
        var cachedCompilation = workspace.CreateAnalysisCompilation(projectId);

        cachedCompilation.ShouldBeSameAs(initialCompilation);

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/a.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(
                """
                func Main() -> () {
                    val value: MissingType = 43
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.CreateAnalysisCompilation(projectId);
        updatedCompilation.ShouldNotBeSameAs(initialCompilation);

        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/a.rav");
        updatedCompilation.GetDiagnostics(updatedTree).ShouldNotBeEmpty();
    }

    [Fact]
    public void CompilationGetSyntaxDiagnostics_SyntaxTree_ExcludesSemanticDiagnostics()
    {
        var tree = SyntaxTree.ParseText(
            """
            func Main() -> () {
                val value: MissingType = 42
            }
            """,
            path: "/tmp/a.rav");

        var compilation = Compilation.Create(
            "test",
            [tree],
            [.. TestMetadataReferences.Default],
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetSyntaxDiagnostics(tree);

        diagnostics.ShouldBeEmpty();
    }

    [Fact]
    public void WorkspaceGetDocumentSyntaxDiagnostics_ReturnsOnlySyntaxDiagnosticsForRequestedDocument()
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
            "a.rav",
            SourceText.From(
                """
                func Main() -> () {
                    val value: MissingType = 42
                }
                """),
            "/tmp/a.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var documentA = workspace.CurrentSolution.Projects.Single().Documents.Single(document => document.FilePath == "/tmp/a.rav");
        var diagnostics = workspace.GetDocumentSyntaxDiagnostics(projectId, documentA.Id);

        diagnostics.ShouldBeEmpty();
    }
}
