namespace Raven.Editor.Tests;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Terminal.Gui;

public class CompletionTests
{
    [Fact]
    public void ApplyCompletion_ReplacesSpanAndPositionsCursor()
    {
        Application.Init(new FakeDriver());

        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject("TestProject");
        var documentId = DocumentId.CreateNew(projectId);

        var editor = new TestCodeTextView(workspace, projectId, documentId)
        {
            Text = "abc"
        };

        var item = new CompletionItem("foo", "foo", new TextSpan(1, 1), CursorOffset: 1);

        Program.ApplyCompletion(editor, item);

        editor.Text!.ToString().ShouldBe("afooc");
        editor.CursorPosition.ShouldBe(new Point(2, 0));

        Application.Shutdown();
    }
    private sealed class TestCodeTextView : CodeTextView
    {
        public TestCodeTextView(RavenWorkspace workspace, ProjectId projectId, DocumentId documentId)
            : base(workspace, projectId, documentId)
        {
        }

        public override void OnContentsChanged()
        {
            // Skip heavy workspace updates for unit tests
        }
    }
}
