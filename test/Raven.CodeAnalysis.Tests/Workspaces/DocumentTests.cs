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
        var solutionId = SolutionId.CreateNew();
        var projectId = ProjectId.CreateNew(solutionId);
        var documentId = DocumentId.CreateNew(projectId);
        var tree = SyntaxTree.ParseText(source, path: "Test.rvn");
        var document = new Document(documentId, "Test.rvn", source, tree, null, VersionStamp.Create());

        var tree1 = await document.GetSyntaxTreeAsync();
        var tree2 = await document.GetSyntaxTreeAsync();
        Assert.NotNull(tree1);
        Assert.Same(tree1, tree2);
    }

    [Fact]
    public void NonRavenDocument_ShouldNotHaveSyntaxTree()
    {
        var solution = new Solution();
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Readme.txt", SourceText.From("Hello"));

        var doc = solution.GetDocument(docId)!;
        var tree = doc.GetSyntaxTreeAsync().Result;
        Assert.Null(tree);
    }
}
