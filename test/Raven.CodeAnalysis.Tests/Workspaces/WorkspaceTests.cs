using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
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

        var updatedText = SourceText.From("print(2);");
        var updatedDoc = originalDoc.WithText(updatedText);
        solution = solution.WithDocument(updatedDoc);
        workspace.TryApplyChanges(solution);

        var finalDoc = workspace.CurrentSolution.GetDocument(docId)!;
        Assert.Equal(docId, finalDoc.Id);
        Assert.NotEqual(originalVersion, finalDoc.Version);
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
}
