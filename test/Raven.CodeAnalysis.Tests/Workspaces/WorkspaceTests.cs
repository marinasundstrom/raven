using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using System.Linq;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class WorkspaceTest
{
    [Fact]
    public void AddProjectAndDocument_ShouldPreserveIds()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "MyProject");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", SourceText.From("class Test {}"));
        workspace.TryApplyChanges(solution);

        var retrieved = workspace.CurrentSolution.GetDocument(docId);
        Assert.Equal("Test.rvn", retrieved?.Name);
        Assert.Equal(docId, retrieved?.Id);
    }

    [Fact]
    public async Task UpdateDocumentText_ShouldCreateNewVersion()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "Project");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Main.rvn", SourceText.From("print(1);"));
        workspace.TryApplyChanges(solution);

        var originalDoc = workspace.CurrentSolution.GetDocument(docId)!;
        var originalVersion = originalDoc.Version;
        var originalProjectVersion = workspace.CurrentSolution.GetProject(projectId)!.Version;
        var originalSolutionVersion = workspace.CurrentSolution.Version;

        var updatedText = SourceText.From("print(2);");
        var updatedDoc = originalDoc.WithText(updatedText);
        solution = solution.WithDocument(updatedDoc);
        workspace.TryApplyChanges(solution);

        var finalDoc = workspace.CurrentSolution.GetDocument(docId)!;
        Assert.Equal(docId, finalDoc.Id);
        Assert.NotEqual(originalVersion, finalDoc.Version);
        Assert.NotEqual(originalProjectVersion, workspace.CurrentSolution.GetProject(projectId)!.Version);
        Assert.NotEqual(originalSolutionVersion, workspace.CurrentSolution.Version);
        Assert.Equal(updatedText.ToString(), (await finalDoc.GetTextAsync()).ToString());
    }

    [Fact]
    public void WorkspaceEvents_ShouldRaiseOnDocumentChange()
    {
        var triggered = false;
        var workspace = new AdhocWorkspace();
        workspace.WorkspaceChanged += (_, args) =>
        {
            if (args.Kind == WorkspaceChangeKind.DocumentChanged)
                triggered = true;
        };

        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Code.rvn", SourceText.From("x = 1"));
        workspace.TryApplyChanges(solution);

        var doc = workspace.CurrentSolution.GetDocument(docId)!;
        var updated = doc.WithText(SourceText.From("x = 2"));
        var newSolution = solution.WithDocument(updated);
        workspace.TryApplyChanges(newSolution);

        Assert.True(triggered);
    }

    [Fact]
    public void GetCompilation_ShouldReuseUnchangedTrees()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var doc1 = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(doc1, "A.rvn", SourceText.From("a = 1"));

        var doc2 = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(doc2, "B.rvn", SourceText.From("b = 2"));

        workspace.TryApplyChanges(solution);

        var comp1 = workspace.GetCompilation(projectId);
        var treeB1 = comp1.SyntaxTrees.Single(t => t.FilePath == "B.rvn");

        var comp2 = workspace.GetCompilation(projectId);
        Assert.Same(comp1, comp2);

        // update first document only
        var doc = workspace.CurrentSolution.GetDocument(doc1)!;
        var updated = doc.WithText(SourceText.From("a = 3"));
        solution = workspace.CurrentSolution.WithDocument(updated);
        workspace.TryApplyChanges(solution);

        var comp3 = workspace.GetCompilation(projectId);
        Assert.NotSame(comp1, comp3);
        var treeB2 = comp3.SyntaxTrees.Single(t => t.FilePath == "B.rvn");
        Assert.Same(treeB1, treeB2);
    }

    [Fact]
    public void WorkspaceEvents_ShouldRaiseOnProjectAdded()
    {
        WorkspaceChangeEventArgs? args = null;
        var workspace = new AdhocWorkspace();
        workspace.WorkspaceChanged += (_, e) => args = e;

        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        var newSolution = solution.AddProject(projectId, "P");
        workspace.TryApplyChanges(newSolution);

        Assert.NotNull(args);
        Assert.Equal(WorkspaceChangeKind.ProjectAdded, args!.Kind);
        Assert.Equal(projectId, args.ProjectId);
        Assert.Null(args.DocumentId);
    }

    [Fact]
    public void WorkspaceEvents_ShouldRaiseOnDocumentAdded()
    {
        WorkspaceChangeEventArgs? args = null;
        var workspace = new AdhocWorkspace();
        workspace.WorkspaceChanged += (_, e) => args = e;

        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        var newSolution = solution.AddDocument(docId, "Code.rvn", SourceText.From("x = 1"));
        workspace.TryApplyChanges(newSolution);

        Assert.NotNull(args);
        Assert.Equal(WorkspaceChangeKind.DocumentAdded, args!.Kind);
        Assert.Equal(projectId, args.ProjectId);
        Assert.Equal(docId, args.DocumentId);
    }

    [Fact]
    public void TryApplyChanges_SameSolution_ShouldNotRaiseEvents()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var triggered = false;
        workspace.WorkspaceChanged += (_, __) => triggered = true;

        workspace.TryApplyChanges(solution);

        Assert.False(triggered);
    }
}
