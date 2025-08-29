using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Terminal.Gui;

namespace Raven.Editor;

internal class Program
{
    private static readonly string[] Keywords = Enum.GetValues(typeof(SyntaxKind))
        .Cast<SyntaxKind>()
        .Where(SyntaxFacts.IsReservedWordKind)
        .Select(k => SyntaxFacts.GetSyntaxTokenText(k)!)
        .ToArray();

    private static readonly RavenWorkspace Workspace = RavenWorkspace.Create();
    private static ProjectId _projectId;
    private static DocumentId _documentId;

    public static void Main(string[] args)
    {
        Application.Init();

        var filePath = args.Length > 0 ? args[0] : "";
        var text = File.Exists(filePath)
            ? File.ReadAllText(filePath)
            : "import System.Console.*\nWriteLine(\"Hello, World!\")\n";
        var sourceText = SourceText.From(text);
        var options = new CompilationOptions(OutputKind.ConsoleApplication);

        _projectId = Workspace.AddProject("EditorProject", compilationOptions: options);
        _documentId = DocumentId.CreateNew(_projectId);
        var solution = Workspace.CurrentSolution.AddDocument(_documentId, "main.rav", sourceText);

        var version = TargetFrameworkResolver.ResolveVersion();
        foreach (var path in TargetFrameworkResolver.GetReferenceAssemblies(version))
            solution = solution.AddMetadataReference(_projectId, MetadataReference.CreateFromFile(path));

        Workspace.TryApplyChanges(solution);

        var editor = new CodeTextView(Workspace, _projectId, _documentId)
        {
            Text = text,
            Width = Dim.Fill(),
            Height = Dim.Fill(),
            WordWrap = false
        };

        var win = new Window("Raven Editor")
        {
            X = 0,
            Y = 0,
            Width = Dim.Fill(),
            Height = Dim.Fill()
        };
        win.Add(editor);
        Application.Top.Add(win);

        editor.KeyPress += e =>
        {
            if (e.KeyEvent.Key == (Key.CtrlMask | Key.S))
            {
                if (!string.IsNullOrEmpty(filePath))
                    File.WriteAllText(filePath, editor.Text.ToString());
                e.Handled = true;
            }
            else if (e.KeyEvent.Key == (Key.CtrlMask | Key.Space))
            {
                ShowCompletion(editor);
                e.Handled = true;
            }
            else if (e.KeyEvent.Key == Key.F5)
            {
                Compile(editor.Text?.ToString() ?? string.Empty);
                e.Handled = true;
            }
        };

        Application.Run();
        Application.Shutdown();
    }

    private static void ShowCompletion(CodeTextView editor)
    {
        var lines = editor.Text?.ToString()?.Split('\n') ?? System.Array.Empty<string>();
        if (editor.CurrentRow >= lines.Length)
            return;
        var line = lines[editor.CurrentRow];
        var col = Math.Min(editor.CurrentColumn, line.Length);
        var start = col;
        while (start > 0 && char.IsLetter(line[start - 1]))
            start--;
        var prefix = line[start..col];
        var matches = Keywords.Where(k => k.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)).OrderBy(k => k).ToArray();
        if (matches.Length == 0)
            return;

        var list = new ListView(matches)
        {
            Width = Dim.Fill(),
            Height = Dim.Fill()
        };
        var ok = new Button("Ok", true);
        var dialog = new Dialog("Completions", 40, 10, ok);
        dialog.Add(list);
        ok.Clicked += () => Application.RequestStop();
        list.OpenSelectedItem += args => Application.RequestStop();
        Application.Run(dialog);

        if (list.SelectedItem >= 0 && list.SelectedItem < matches.Length)
        {
            var selected = matches[list.SelectedItem];
            editor.InsertText(selected.Substring(prefix.Length));
        }
    }

    private static void Compile(string source)
    {
        var solution = Workspace.CurrentSolution.WithDocumentText(_documentId, SourceText.From(source));
        Workspace.TryApplyChanges(solution);
        var diagnostics = Workspace.GetDiagnostics(_projectId);
        if (diagnostics.IsDefaultOrEmpty)
        {
            MessageBox.Query("Compilation", "Compilation succeeded", "Ok");
        }
        else
        {
            var text = string.Join('\n', diagnostics.Select(d => d.ToString()));
            MessageBox.ErrorQuery("Compilation", text, "Ok");
        }
    }
}
