using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Terminal.Gui;

using Attribute = Terminal.Gui.Attribute;
using ColorScheme = Terminal.Gui.ColorScheme;

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
    private static CodeTextView? _editor;

    public static void Main(string[] args)
    {
        Application.Init();
        Console.CancelKeyPress += HandleCancelKeyPress;

        var filePath = args.Length > 0 ? args[0] : string.Empty;
        var documentName = string.IsNullOrEmpty(filePath) ? "main.rav" : Path.GetFileName(filePath);
        var text = File.Exists(filePath)
            ? File.ReadAllText(filePath)
            : "import System.Console.*\n\nWriteLine(\"Hello, World!\")\n";
        var sourceText = SourceText.From(text);
        var options = new CompilationOptions(OutputKind.ConsoleApplication);

        _projectId = Workspace.AddProject("EditorProject", compilationOptions: options);
        _documentId = DocumentId.CreateNew(_projectId);
        var solution = Workspace.CurrentSolution.AddDocument(_documentId, documentName, sourceText);

        var targetFrameworkTfm = "net9.0";

        var targetFramework = targetFrameworkTfm ?? TargetFrameworkUtil.GetLatestFramework();
        var version = TargetFrameworkResolver.ResolveVersion(targetFramework);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        foreach (var refPath in new[]
        {
            Path.Combine(refAssembliesPath!, "System.Runtime.dll"),
            Path.Combine(refAssembliesPath!, "System.Collections.dll"),
            typeof(Console).Assembly.Location,
            //Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "../../../TestDep.dll"))
        })
        {
            var reference = MetadataReference.CreateFromFile(Path.GetFullPath(refPath));
            solution = solution.AddMetadataReference(_projectId, reference);
        }

        Workspace.TryApplyChanges(solution);

        var editorScheme = new ColorScheme
        {
            Normal = new Attribute(Color.Black, Color.White),
            Focus = new Attribute(Color.Black, Color.White),
            HotNormal = new Attribute(Color.Blue, Color.White),
            HotFocus = new Attribute(Color.Blue, Color.White),
            Disabled = new Attribute(Color.DarkGray, Color.White)
        };

        var editor = new CodeTextView(Workspace, _projectId, _documentId)
        {
            Text = text,
            Width = Dim.Fill(),
            Height = Dim.Fill(10),
            WordWrap = false,
            ColorScheme = editorScheme
        };

        _editor = editor;

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

        var windowTitle = string.IsNullOrEmpty(filePath)
            ? "Raven Editor"
            : $"Raven Editor - {documentName}";

        var win = new Window(windowTitle)
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
                _completionList.EnsureSelectedItemVisible();
                _completionList.SetNeedsDisplay();
                e.Handled = true;
            }
            else if (_completionWin != null && e.KeyEvent.Key == Key.CursorUp)
            {
                _completionList!.SelectedItem = Math.Max(_completionList.SelectedItem - 1, 0);
                _completionList.EnsureSelectedItemVisible();
                _completionList.SetNeedsDisplay();
                e.Handled = true;
            }
            else if (_completionWin != null && (e.KeyEvent.Key == Key.Enter || e.KeyEvent.Key == Key.Tab))
            {
                if (_currentItems.Length > 0)
                {
                    var item = _currentItems[_completionList!.SelectedItem];
                    ApplyCompletion(editor, item);
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
            else if (e.KeyEvent.Key == (Key.CtrlMask | Key.C))
            {
                Application.RequestStop();
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

    internal static void HandleCancelKeyPress(object? sender, ConsoleCancelEventArgs e)
    {
        e.Cancel = true;
        Application.RequestStop();
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

        var sourceText = SourceText.From(text);
        var solution = Workspace.CurrentSolution.WithDocumentText(_documentId, sourceText);
        Workspace.TryApplyChanges(solution);
        var document = Workspace.CurrentSolution.GetDocument(_documentId)!;
        var tree = document.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;
        var compilation = Workspace.GetCompilation(_projectId);

        _currentItems = CompletionService
            .GetCompletions(compilation, tree, position)
            .Where(i => !string.IsNullOrWhiteSpace(i.DisplayText))
            .ToArray();
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
            var completionScheme = new ColorScheme
            {
                Normal = new Attribute(Color.Black, Color.Gray),
                Focus = new Attribute(Color.Black, Color.DarkGray),
                HotNormal = new Attribute(Color.Blue, Color.Gray),
                HotFocus = new Attribute(Color.Blue, Color.DarkGray),
                Disabled = new Attribute(Color.DarkGray, Color.Gray)
            };

            _completionList = new ListView(_currentCompletions)
            {
                CanFocus = false,
                Width = Dim.Fill(),              // <-- critical
                Height = Dim.Fill(),             // <-- critical
                ColorScheme = completionScheme
            };

            _completionList.OpenSelectedItem += _ =>
            {
                if (_currentItems.Length > 0)
                {
                    var item = _currentItems[_completionList.SelectedItem];
                    ApplyCompletion(editor, item);
                    HideCompletion();
                }
            };

            _completionList.KeyPress += e =>
            {
                if (_completionWin != null && e.KeyEvent.Key == Key.CursorDown)
                {
                    _completionList!.SelectedItem =
                        Math.Min(_completionList.SelectedItem + 1, _currentCompletions.Length - 1);

                    _completionList.EnsureSelectedItemVisible();        // keep it in view
                    _completionList.SetNeedsDisplay();                           // repaint
                    e.Handled = true;
                }
                else if (_completionWin != null && e.KeyEvent.Key == Key.CursorUp)
                {
                    _completionList!.SelectedItem =
                        Math.Max(_completionList.SelectedItem - 1, 0);

                    _completionList.EnsureSelectedItemVisible();
                    _completionList.SetNeedsDisplay();
                    e.Handled = true;
                }
                else if (e.KeyEvent.Key == Key.Enter || e.KeyEvent.Key == Key.Tab)
                {
                    var item = _currentItems[_completionList.SelectedItem];
                    ApplyCompletion(editor, item);
                    HideCompletion();
                    e.Handled = true;
                }
                else if (e.KeyEvent.Key == Key.Esc)
                {
                    HideCompletion();
                    e.Handled = true;
                }
            };

            _completionWin = new Window
            {
                Width = width,
                Height = height,
                ColorScheme = completionScheme
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

        // after creating/updating the popup:
        _completionList.SelectedItem = 0;

        _completionWin.X = editor.Frame.X + start;
        _completionWin.Y = editor.Frame.Y + editor.CurrentRow + 1;
        _completionWin.SetNeedsDisplay();
    }

    internal static void ApplyCompletion(CodeTextView editor, CompletionItem item)
    {
        var text = editor.Text?.ToString() ?? string.Empty;
        var before = text[..item.ReplacementSpan.Start];
        var after = text[item.ReplacementSpan.End..];
        var newText = before + item.InsertionText + after;
        editor.Text = newText;

        var cursorPos = item.ReplacementSpan.Start + (item.CursorOffset ?? item.InsertionText.Length);
        var lines = newText.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n');
        var position = 0;
        for (var row = 0; row < lines.Length; row++)
        {
            var lineLength = lines[row].Length;
            if (cursorPos <= position + lineLength)
            {
                editor.CursorPosition = new Point(cursorPos - position, row);
                break;
            }
            position += lineLength + 1;
        }
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
            if (_editor != null)
            {
                _editor.SetFocus();
                Application.Driver?.SetCursorVisibility(CursorVisibility.Default);
            }
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
