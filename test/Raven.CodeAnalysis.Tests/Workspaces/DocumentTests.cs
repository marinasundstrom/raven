using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class DocumentTests
{
    [Fact]
    public async Task GetSyntaxTreeAsync_ShouldReturnSameInstance()
    {
        var source = SourceText.From("x = 1");
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Test.rvn", source);
        var document = solution.GetDocument(documentId)!;

        var tree1 = await document.GetSyntaxTreeAsync();
        var tree2 = await document.GetSyntaxTreeAsync();
        Assert.NotNull(tree1);
        Assert.Same(tree1, tree2);
    }

    [Fact]
    public void NonRavenDocument_ShouldNotHaveSyntaxTree()
    {
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Readme.txt", SourceText.From("Hello"));

        var doc = solution.GetDocument(docId)!;
        var tree = doc.GetSyntaxTreeAsync().Result;
        Assert.Null(tree);
    }

    [Fact]
    public void SupportsSyntaxTree_ShouldReflectFileExtension()
    {
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId1 = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId1, "Test.rvn", SourceText.From("x = 1"));
        var docId2 = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId2, "Readme.txt", SourceText.From("Hello"));

        var doc1 = solution.GetDocument(docId1)!;
        var doc2 = solution.GetDocument(docId2)!;

        Assert.True(doc1.SupportsSyntaxTree);
        Assert.False(doc2.SupportsSyntaxTree);
        Assert.True(doc1.SupportsSemanticModel);
        Assert.False(doc2.SupportsSemanticModel);
    }

    [Fact]
    public async Task WithSyntaxRoot_ShouldUpdateTreeAndText()
    {
        var source = SourceText.From("x = 1");
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", source);
        var document = solution.GetDocument(docId)!;

        var newTree = SyntaxFactory.ParseSyntaxTree("x = 2");
        var newDoc = document.WithSyntaxRoot(newTree.GetRoot());

        var updatedText = await newDoc.GetTextAsync();
        Assert.Equal("x = 2", updatedText.ToString());
        var tree = await newDoc.GetSyntaxTreeAsync();
        Assert.Same(newTree, tree);
    }

    [Fact]
    public async Task GetSemanticModelAsync_ShouldReturnModel()
    {
        var source = SourceText.From("val x = 1");
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", source);
        var document = solution.GetDocument(docId)!;

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var project = document.Project.AddMetadataReference(
            MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")));

        var newDoc = project.GetDocument(docId)!;

        var model = await newDoc.GetSemanticModelAsync();
        Assert.NotNull(model);
    }

    [Fact]
    public async Task GetTextChangesAsync_ShouldReturnChanges()
    {
        var source = SourceText.From("x = 1");
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", source);
        var document = solution.GetDocument(docId)!;

        var newDoc = document.WithText(SourceText.From("x = 2"));

        var changes = await newDoc.GetTextChangesAsync(document);
        Assert.Single(changes);
    }

    [Fact]
    public async Task WithText_ShouldSeedIncrementalSyntaxTreeWhenOriginalTreeWasCached()
    {
        var source = SourceText.From(
            """
            val x = 1
            val y = 2
            """);
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", source);
        var document = solution.GetDocument(docId)!;

        var originalTree = await document.GetSyntaxTreeAsync();
        Assert.NotNull(originalTree);

        var updatedText = SourceText.From(
            """
            val x = 3
            val y = 2
            """);
        var updatedDocument = document.WithText(updatedText);

        var cachedTree = GetCachedSyntaxTree(updatedDocument);
        Assert.NotNull(cachedTree);
        Assert.Equal(updatedText.ToString(), cachedTree!.GetText().ToString());

        var updatedTree = await updatedDocument.GetSyntaxTreeAsync();
        Assert.Same(cachedTree, updatedTree);
    }

    [Fact]
    public async Task GetTextVersionAsync_ShouldReturnVersion()
    {
        var source = SourceText.From("x = 1");
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", source);
        var document = solution.GetDocument(docId)!;

        var version = await document.GetTextVersionAsync();
        Assert.Equal(document.Version, version);
    }

    [Fact]
    public async Task GetSemanticModelAsync_StaleWorkspaceDocument_RebindsToCurrentCompilationTree()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("App");
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        var document = project.AddDocument("test.rvn", SourceText.From("val x = 1"));
        workspace.TryApplyChanges(document.Project.Solution);

        var staleDocument = workspace.CurrentSolution.GetDocument(document.Id)!;
        _ = await staleDocument.GetSyntaxTreeAsync();

        var updatedSolution = workspace.CurrentSolution.WithDocumentText(document.Id, SourceText.From("val y = 2"));
        workspace.TryApplyChanges(updatedSolution);

        var model = await staleDocument.GetSemanticModelAsync();
        model.ShouldNotBeNull();
    }

    [Fact]
    public async Task GetExpandedSyntaxRootAsync_StaleWorkspaceDocument_RebindsToCurrentCompilationTree()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("App");
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        var document = project.AddDocument("test.rvn", SourceText.From("val x = 1"));
        workspace.TryApplyChanges(document.Project.Solution);

        var staleDocument = workspace.CurrentSolution.GetDocument(document.Id)!;
        _ = await staleDocument.GetSyntaxTreeAsync();

        var updatedSolution = workspace.CurrentSolution.WithDocumentText(document.Id, SourceText.From("val y = 2"));
        workspace.TryApplyChanges(updatedSolution);

        var root = await staleDocument.GetExpandedSyntaxRootAsync();
        root.ShouldNotBeNull();
    }

    private static SyntaxTree? GetCachedSyntaxTree(Document document)
    {
        var documentInfoField = typeof(Document).GetField("_info", BindingFlags.Instance | BindingFlags.NonPublic);
        var documentInfo = documentInfoField?.GetValue(document);
        Assert.NotNull(documentInfo);

        var syntaxTreeField = documentInfo!.GetType().GetField("_syntaxTree", BindingFlags.Instance | BindingFlags.NonPublic);
        return syntaxTreeField?.GetValue(documentInfo) as SyntaxTree;
    }
}
