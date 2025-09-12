namespace Raven.Editor.Tests;

using System;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Terminal.Gui;

public class ProgramTests
{
    [Fact]
    public void HandleCancelKeyPress_SetsCancel()
    {
        Application.Init(new FakeDriver());

        var e = (ConsoleCancelEventArgs)Activator.CreateInstance(
            typeof(ConsoleCancelEventArgs),
            BindingFlags.Instance | BindingFlags.NonPublic,
            null,
            new object[] { ConsoleSpecialKey.ControlC },
            null)!;

        Program.HandleCancelKeyPress(null, e);

        e.Cancel.ShouldBeTrue();

        Application.Shutdown();
    }

    [Fact]
    public void HideCompletion_RefocusesEditor()
    {
        Application.Init(new FakeDriver());

        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject("TestProject");
        var documentId = DocumentId.CreateNew(projectId);

        var editor = new TestCodeTextView(workspace, projectId, documentId);
        Application.Top.Add(editor);
        editor.SetFocus();

        var completionWin = new Window();
        Application.Top.Add(completionWin);
        completionWin.SetFocus();

        typeof(Program).GetField("_editor", BindingFlags.NonPublic | BindingFlags.Static)!
            .SetValue(null, editor);
        typeof(Program).GetField("_completionWin", BindingFlags.NonPublic | BindingFlags.Static)!
            .SetValue(null, completionWin);

        typeof(Program).GetMethod("HideCompletion", BindingFlags.NonPublic | BindingFlags.Static)!
            .Invoke(null, null);

        Application.Top.Focused.ShouldBe(editor);
        Application.Driver!.GetCursorVisibility(out var visibility);
        visibility.ShouldBe(CursorVisibility.Default);

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
