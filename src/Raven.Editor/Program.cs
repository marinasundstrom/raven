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
    private static readonly RavenWorkspace Workspace = RavenWorkspace.Create();
    private static readonly CompletionService CompletionService = new();
    private static ProjectId _projectId;
    private static DocumentId _documentId;
    private static readonly List<string> Output = new();
    private static readonly List<string> Problems = new();
    private static ListView? _outputView;
    private static ListView? _problemsView;
    private static Window? _completionWin;
    private static ListView? _completionList;
    private static CompletionItem[] _currentItems = Array.Empty<CompletionItem>();
    private static string[] _currentCompletions = Array.Empty<string>();
    private static string _currentPrefix = string.Empty;

    public static void Main(string[] args)
    {
        Application.Init();

        var filePath = args.Length > 0 ? args[0] : "";
        var text = File.Exists(filePath)
            ? File.ReadAllText(filePath)
            : "import System.Console.*\n\nWriteLine(\"Hello, World!\")\n";
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
            Height = Dim.Fill(10),
            WordWrap = false
        };

        _outputView = new ListView(Output) { Width = Dim.Fill(), Height = Dim.Fill() };
        _problemsView = new ListView(Problems) { Width = Dim.Fill(), Height = Dim.Fill() };

        var problemsFrame = new FrameView("Problems")
        {
            X = 0,
            Y = Pos.AnchorEnd(10),
            Width = Dim.Fill(),
            Height = 5
        };
        problemsFrame.Add(_problemsView);

        var outputFrame = new FrameView("Output")
        {
            X = 0,
            Y = Pos.AnchorEnd(5),
            Width = Dim.Fill(),
            Height = 5
        };
        outputFrame.Add(_outputView);

        var win = new Window("Raven Editor")
        {
            X = 0,
            Y = 0,
            Width = Dim.Fill(),
            Height = Dim.Fill()
        };
        win.Add(editor, problemsFrame, outputFrame);
        Application.Top.Add(win);

        editor.KeyPress += e =>
        {
            if (_completionWin != null && e.KeyEvent.Key == Key.CursorDown)
            {
                _completionList!.SelectedItem = Math.Min(_completionList.SelectedItem + 1, _currentCompletions.Length - 1);
                e.Handled = true;
            }
            else if (_completionWin != null && e.KeyEvent.Key == Key.CursorUp)
            {
                _completionList!.SelectedItem = Math.Max(_completionList.SelectedItem - 1, 0);
                e.Handled = true;
            }
            else if (_completionWin != null && (e.KeyEvent.Key == Key.Enter || e.KeyEvent.Key == Key.Tab))
            {
                if (_currentItems.Length > 0)
                {
                    var item = _currentItems[_completionList!.SelectedItem];
                    var insertion = item.InsertionText;
                    if (insertion.StartsWith(_currentPrefix, StringComparison.OrdinalIgnoreCase))
                        insertion = insertion[_currentPrefix.Length..];
                    editor.InsertText(insertion);
                }
                HideCompletion();
                e.Handled = true;
            }
            else if (_completionWin != null && e.KeyEvent.Key == Key.Esc)
            {
                HideCompletion();
                e.Handled = true;
            }
            else if (e.KeyEvent.Key == (Key.CtrlMask | Key.S))
            {
                if (!string.IsNullOrEmpty(filePath))
                    File.WriteAllText(filePath, editor.Text.ToString());
                e.Handled = true;
            }
            else if (e.KeyEvent.Key == Key.F5)
            {
                Compile(editor.Text?.ToString() ?? string.Empty);
                e.Handled = true;
            }
            else if (!e.KeyEvent.IsCtrl && !e.KeyEvent.IsAlt && e.KeyEvent.KeyValue >= 32 && e.KeyEvent.KeyValue <= 126)
            {
                Application.MainLoop.AddIdle(() =>
                {
                    ShowCompletion(editor);
                    return false;
                });
            }
        };

        Application.Run();
        Application.Shutdown();
    }

    private static void ShowCompletion(CodeTextView editor)
    {
        var text = editor.Text?.ToString() ?? string.Empty;
        var lines = text.Split('\n');
        if (editor.CurrentRow >= lines.Length)
        {
            HideCompletion();
            return;
        }

        var line = lines[editor.CurrentRow];
        var col = Math.Min(editor.CurrentColumn, line.Length);
        var position = 0;
        for (var i = 0; i < editor.CurrentRow; i++)
            position += lines[i].Length + 1;
        position += col;

        var start = col;
        while (start > 0 && char.IsLetter(line[start - 1]))
            start--;
        _currentPrefix = line[start..col];

        var sourceText = SourceText.From(text);
        var solution = Workspace.CurrentSolution.WithDocumentText(_documentId, sourceText);
        Workspace.TryApplyChanges(solution);
        var document = Workspace.CurrentSolution.GetDocument(_documentId)!;
        var tree = document.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;
        var compilation = Workspace.GetCompilation(_projectId);

        _currentItems = CompletionService.GetCompletions(compilation, tree, position).ToArray();
        if (_currentItems.Length == 0)
        {
            HideCompletion();
            return;
        }

        _currentCompletions = _currentItems.Select(i => i.DisplayText).ToArray();

        var height = Math.Min(10, _currentCompletions.Length + 1);
        var width = Math.Max(20, _currentCompletions.Max(s => s.Length) + 2);

        if (_completionWin == null)
        {
            _completionList = new ListView(_currentCompletions) { CanFocus = false };
            _completionWin = new Window
            {
                Width = width,
                Height = height
            };
            _completionWin.Add(_completionList);
            Application.Top.Add(_completionWin);
        }
        else
        {
            _completionList!.SetSource(_currentCompletions);
            _completionWin.Width = width;
            _completionWin.Height = height;
        }

        _completionList!.SelectedItem = 0;
        _completionWin.X = editor.Frame.X + start;
        _completionWin.Y = editor.Frame.Y + editor.CurrentRow + 1;
        _completionWin.SetNeedsDisplay();
    }

    private static void HideCompletion()
    {
        if (_completionWin != null)
        {
            Application.Top.Remove(_completionWin);
            _completionWin = null;
            _completionList = null;
            _currentItems = Array.Empty<CompletionItem>();
            _currentCompletions = Array.Empty<string>();
            _currentPrefix = string.Empty;
        }
    }

    private static void Compile(string source)
    {
        try
        {
            var solution = Workspace.CurrentSolution.WithDocumentText(_documentId, SourceText.From(source));
            Workspace.TryApplyChanges(solution);
            var diagnostics = Workspace.GetDiagnostics(_projectId);
            Output.Clear();
            Problems.Clear();
            if (diagnostics.IsDefaultOrEmpty)
            {
                Output.Add("Compilation succeeded");
            }
            else
            {
                Output.Add("Compilation failed");
                Problems.AddRange(diagnostics.Select(d => d.ToString()));
            }
            _outputView!.SetSource(Output);
            _problemsView!.SetSource(Problems);
        }
        catch (Exception ex)
        {
            Output.Clear();
            Problems.Clear();
            Output.Add("Compilation failed");
            Problems.Add(ex.ToString());
            _outputView!.SetSource(Output);
            _problemsView!.SetSource(Problems);
        }
    }
}
