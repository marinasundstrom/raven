using System.Threading.Tasks;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
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
        var source = SourceText.From("x = 1");
        var solution = new Solution(HostServices.Default);
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", source);
        var document = solution.GetDocument(docId)!;

        var model = await document.GetSemanticModelAsync();
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
}
