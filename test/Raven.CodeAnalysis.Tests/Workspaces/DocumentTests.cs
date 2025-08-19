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
}
