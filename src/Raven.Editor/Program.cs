using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

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
    private static readonly object WorkspaceGate = new();
    private static CancellationTokenSource? _completionCts;
    private static ProjectId _projectId;
    private static DocumentId _documentId;
    private static readonly List<string> Output = new();
    private static readonly List<string> Problems = new();
    private static ListView? _outputView;
    private static ListView? _problemsView;
    private static Window? _mainWindow;
    private static Window? _completionWin;
    private static ListView? _completionList;
    private static CompletionItem[] _currentItems = Array.Empty<CompletionItem>();
    private static string[] _currentCompletions = Array.Empty<string>();
    private static CodeTextView? _editor;
    private const int CompletionDebounceMs = 40;
    private readonly record struct CompletionContext(string Text, int Row, int Start, int Position);
    private static string _baseWindowTitle = "Raven Editor";

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

        var frameworkReferences = TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();

        foreach (var reference in frameworkReferences)
        {
            solution = solution.AddMetadataReference(_projectId, reference);
        }

        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new EventDelegateMustBeNullableAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new NonNullDeclarationsAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new VarCanBeValAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new MatchExhaustivenessAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new PreferValInsteadOfLetAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new AutoPropertyInitializationAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new ThrowStatementUseResultAnalyzer()));
        solution = solution.AddAnalyzerReference(_projectId, new AnalyzerReference(new PreferDuLinqExtensionsAnalyzer()));

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
        _baseWindowTitle = windowTitle;

        var win = new Window(windowTitle)
        {
            X = 0,
            Y = 1,
            Width = Dim.Fill(),
            Height = Dim.Fill(1)
        };
        _mainWindow = win;
        win.Add(editor, problemsFrame, outputFrame);

        var menu = new MenuBar(new[]
        {
            new MenuBarItem("_File", new[]
            {
                new MenuItem("_Save", string.Empty, () =>
                {
                    SaveDocument(filePath, editor);
                }, null, null, Key.CtrlMask | Key.S),
                new MenuItem("_Quit", string.Empty, () => Application.RequestStop(), null, null, Key.CtrlMask | Key.C)
            }),
            new MenuBarItem("_Build", new[]
            {
                new MenuItem("_Compile", string.Empty, () => Compile(editor.Text?.ToString() ?? string.Empty), null, null, Key.F7),
                new MenuItem("Compile and _Run", string.Empty, () => CompileAndRun(editor.Text?.ToString() ?? string.Empty), null, null, Key.F5)
            }),
            new MenuBarItem("_Edit", new[]
            {
                new MenuItem("_Format", string.Empty, () => FormatDocument(editor), null, null, Key.F8)
            })
        });

        var statusBar = new StatusBar(new[]
        {
            new StatusItem(Key.CtrlMask | Key.S, "~^S~ Save", () =>
            {
                SaveDocument(filePath, editor);
            }),
            new StatusItem(Key.F7, "~F7~ Compile", () =>
                Compile(editor.Text?.ToString() ?? string.Empty)),
            new StatusItem(Key.F5, "~F5~ Run", () =>
                CompileAndRun(editor.Text?.ToString() ?? string.Empty)),
            new StatusItem(Key.F8, "~F8~ Format", () =>
                FormatDocument(editor)),
            new StatusItem(Key.CtrlMask | Key.C, "~^C~ Exit", () => Application.RequestStop())
        });

        Application.Top.Add(menu, win, statusBar);

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
                SaveDocument(filePath, editor);
                e.Handled = true;
            }
            else if (e.KeyEvent.Key == (Key.CtrlMask | Key.C))
            {
                Application.RequestStop();
                e.Handled = true;
            }
            else if (IsBuildShortcut(e.KeyEvent))
            {
                Compile(editor.Text?.ToString() ?? string.Empty);
                e.Handled = true;
            }
            else if (IsRunShortcut(e.KeyEvent))
            {
                CompileAndRun(editor.Text?.ToString() ?? string.Empty);
                e.Handled = true;
            }
            else if (IsFormatShortcut(e.KeyEvent))
            {
                FormatDocument(editor);
                e.Handled = true;
            }
            else if (IsManualCompletionShortcut(e.KeyEvent))
            {
                _completionCts?.Cancel();
                _completionCts?.Dispose();
                _completionCts = null;
                _ = ShowCompletionAsync(editor, CancellationToken.None);
                e.Handled = true;
            }
            else if (e.KeyEvent.Key == Key.F6) // legacy alias
            {
                CompileAndRun(editor.Text?.ToString() ?? string.Empty);
                e.Handled = true;
            }
            else if (IsCompletionTriggerKey(e.KeyEvent))
            {
                ScheduleCompletion(editor, debounceMs: 0);
            }
            else if (!e.KeyEvent.IsCtrl && !e.KeyEvent.IsAlt && e.KeyEvent.KeyValue >= 32 && e.KeyEvent.KeyValue <= 126)
            {
                ScheduleCompletion(editor);
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

    private static async Task ShowCompletionAsync(CodeTextView editor, CancellationToken cancellationToken)
    {
        try
        {
            var context = await InvokeOnMainLoopAsync(() =>
            {
                var text = editor.Text?.ToString() ?? string.Empty;
                var lines = text.Split('\n');
                if (editor.CurrentRow >= lines.Length)
                    return (CompletionContext?)null;

                var line = lines[editor.CurrentRow];
                var col = Math.Min(editor.CurrentColumn, line.Length);
                var position = 0;
                for (var i = 0; i < editor.CurrentRow; i++)
                    position += lines[i].Length + 1;
                position += col;

                var start = col;
                while (start > 0 && char.IsLetter(line[start - 1]))
                    start--;

                return new CompletionContext(text, editor.CurrentRow, start, position);
            }).ConfigureAwait(false);

            if (context is null)
            {
                await InvokeOnMainLoopAsync(HideCompletion).ConfigureAwait(false);
                return;
            }

            cancellationToken.ThrowIfCancellationRequested();

            var completionInputs = await TryGetCompletionInputsAsync(context.Value.Text, cancellationToken).ConfigureAwait(false);
            if (completionInputs is null)
            {
                await InvokeOnMainLoopAsync(HideCompletion).ConfigureAwait(false);
                return;
            }

            var items = (await CompletionService
                    .GetCompletionsAsync(completionInputs.Value.compilation, completionInputs.Value.tree, context.Value.Position, cancellationToken)
                    .ConfigureAwait(false))
                .Where(i => !string.IsNullOrWhiteSpace(i.DisplayText))
                .ToArray();

            cancellationToken.ThrowIfCancellationRequested();

            await InvokeOnMainLoopAsync(() =>
            {
                if (items.Length == 0)
                {
                    HideCompletion();
                    return;
                }

                _currentItems = items;
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
                        Width = Dim.Fill(),
                        Height = Dim.Fill(),
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
                            _completionList.EnsureSelectedItemVisible();
                            _completionList.SetNeedsDisplay();
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

                _completionList.SelectedItem = 0;
                _completionWin.X = editor.Frame.X + context.Value.Start;
                _completionWin.Y = editor.Frame.Y + context.Value.Row + 1;
                _completionWin.SetNeedsDisplay();
            }).ConfigureAwait(false);
        }
        catch
        {
            await InvokeOnMainLoopAsync(HideCompletion).ConfigureAwait(false);
        }
    }

    private static void ShowCompletion(CodeTextView editor)
    {
        _ = ShowCompletionAsync(editor, CancellationToken.None);
    }

    private static void ScheduleCompletion(CodeTextView editor, int debounceMs = CompletionDebounceMs)
    {
        _completionCts?.Cancel();
        _completionCts?.Dispose();

        var cts = new CancellationTokenSource();
        _completionCts = cts;

        _ = Task.Run(async () =>
        {
            try
            {
                if (debounceMs > 0)
                    await Task.Delay(debounceMs, cts.Token);

                if (cts.Token.IsCancellationRequested)
                    return;

                await ShowCompletionAsync(editor, cts.Token).ConfigureAwait(false);
            }
            catch (OperationCanceledException)
            {
                // Ignore canceled completion requests.
            }
        });
    }

    private static async Task<(Compilation compilation, SyntaxTree tree)?> TryGetCompletionInputsAsync(string text, CancellationToken cancellationToken)
    {
        try
        {
            Document? document;
            lock (WorkspaceGate)
            {
                var solution = Workspace.CurrentSolution;
                var project = solution.GetProject(_projectId);
                if (project is null || project.GetDocument(_documentId) is null)
                    return null;

                solution = solution.WithDocumentText(_documentId, SourceText.From(text));
                if (!Workspace.TryApplyChanges(solution))
                    return null;

                document = Workspace.CurrentSolution.GetDocument(_documentId);
            }

            if (document is null)
                return null;

            var tree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (tree is null)
                return null;

            cancellationToken.ThrowIfCancellationRequested();
            Compilation compilation;
            lock (WorkspaceGate)
            {
                compilation = Workspace.GetCompilation(_projectId);
            }

            return (compilation, tree);
        }
        catch
        {
            return null;
        }
    }

    internal static void ApplyCompletion(CodeTextView editor, CompletionItem item)
    {
        var text = editor.Text?.ToString() ?? string.Empty;
        if (item.ReplacementSpan.Start < 0 || item.ReplacementSpan.End < item.ReplacementSpan.Start || item.ReplacementSpan.End > text.Length)
            return;

        var before = text[..item.ReplacementSpan.Start];
        var after = text[item.ReplacementSpan.End..];
        var newText = before + item.InsertionText + after;
        editor.Text = newText;

        var cursorPos = item.ReplacementSpan.Start + (item.CursorOffset ?? item.InsertionText.Length);
        cursorPos = Math.Clamp(cursorPos, 0, newText.Length);
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
        _completionCts?.Cancel();
        _completionCts?.Dispose();
        _completionCts = null;

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
        _ = CompileAsync(source, runAfterCompile: false);
    }

    private static void CompileAndRun(string source)
    {
        _ = CompileAsync(source, runAfterCompile: true);
    }

    private static async Task CompileAsync(string source, bool runAfterCompile)
    {
        string? emittedAssemblyPath = null;
        try
        {
            await InvokeOnMainLoopAsync(() => SetStatus("Compiling...")).ConfigureAwait(false);

            lock (WorkspaceGate)
            {
                var solution = Workspace.CurrentSolution.WithDocumentText(_documentId, SourceText.From(source));
                if (!Workspace.TryApplyChanges(solution))
                    throw new InvalidOperationException("Unable to apply source changes to workspace.");
            }

            var diagnostics = await Task.Run(() => Workspace.GetDiagnostics(_projectId)).ConfigureAwait(false);

            if (runAfterCompile && !diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error))
            {
                await InvokeOnMainLoopAsync(() => SetStatus("Preparing run...")).ConfigureAwait(false);

                Compilation compilation;
                lock (WorkspaceGate)
                {
                    compilation = Workspace.GetCompilation(_projectId);
                }

                emittedAssemblyPath = Path.Combine(Path.GetTempPath(), $"raven-editor-{Guid.NewGuid():N}.dll");
                await using var peStream = File.Open(emittedAssemblyPath, FileMode.Create, FileAccess.Write, FileShare.Read);
                var emitResult = await Task.Run(() => compilation.Emit(peStream)).ConfigureAwait(false);
                diagnostics = diagnostics.Concat(emitResult.Diagnostics).Distinct().ToImmutableArray();

                if (emitResult.Success)
                    CreateRuntimeConfigForAssembly(emittedAssemblyPath);
            }

            await InvokeOnMainLoopAsync(() =>
            {
                Output.Clear();
                Problems.Clear();
                if (!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error))
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
            }).ConfigureAwait(false);

            if (runAfterCompile && !diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error) && emittedAssemblyPath is not null)
            {
                await InvokeOnMainLoopAsync(() => SetStatus("Running program...")).ConfigureAwait(false);
                var (exitCode, stdout, stderr) = await RunAssemblyAsync(emittedAssemblyPath).ConfigureAwait(false);

                await InvokeOnMainLoopAsync(() =>
                {
                    if (!string.IsNullOrWhiteSpace(stdout))
                    {
                        foreach (var line in SplitLines(stdout))
                            Output.Add(line);
                    }

                    Output.Add($"Program exited with code {exitCode}");

                    if (!string.IsNullOrWhiteSpace(stderr))
                    {
                        foreach (var line in SplitLines(stderr))
                            Problems.Add(line);
                    }

                    _outputView!.SetSource(Output);
                    _problemsView!.SetSource(Problems);
                }).ConfigureAwait(false);
            }

            await InvokeOnMainLoopAsync(() =>
                SetStatus(diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error)
                    ? "Compilation failed"
                    : "Ready")).ConfigureAwait(false);
        }
        catch (Exception ex)
        {
            await InvokeOnMainLoopAsync(() =>
            {
                Output.Clear();
                Problems.Clear();
                Output.Add("Compilation failed");
                Problems.Add(ex.ToString());
                _outputView!.SetSource(Output);
                _problemsView!.SetSource(Problems);
                SetStatus("Compilation failed");
            }).ConfigureAwait(false);
        }
        finally
        {
            var runtimeConfigPath = emittedAssemblyPath is null
                ? null
                : Path.ChangeExtension(emittedAssemblyPath, "runtimeconfig.json");

            if (emittedAssemblyPath is not null && File.Exists(emittedAssemblyPath))
            {
                try
                {
                    File.Delete(emittedAssemblyPath);
                }
                catch
                {
                    // Best-effort cleanup for temporary run artifacts.
                }
            }

            if (runtimeConfigPath is not null && File.Exists(runtimeConfigPath))
            {
                try
                {
                    File.Delete(runtimeConfigPath);
                }
                catch
                {
                    // Best-effort cleanup for temporary run artifacts.
                }
            }
        }
    }

    private static async Task<(int ExitCode, string StdOut, string StdErr)> RunAssemblyAsync(string outputFilePath)
    {
        using var process = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                FileName = "dotnet",
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            }
        };
        process.StartInfo.ArgumentList.Add(outputFilePath);

        if (!process.Start())
            throw new InvalidOperationException("Failed to start the produced assembly.");

        var stdoutTask = process.StandardOutput.ReadToEndAsync();
        var stderrTask = process.StandardError.ReadToEndAsync();
        await process.WaitForExitAsync().ConfigureAwait(false);

        var stdout = await stdoutTask.ConfigureAwait(false);
        var stderr = await stderrTask.ConfigureAwait(false);
        return (process.ExitCode, stdout, stderr);
    }

    private static void CreateRuntimeConfigForAssembly(string assemblyPath)
    {
        var runtimeConfigPath = Path.ChangeExtension(assemblyPath, "runtimeconfig.json");
        var runtimeConfig = new
        {
            runtimeOptions = new
            {
                tfm = "net9.0",
                framework = new
                {
                    name = "Microsoft.NETCore.App",
                    version = "9.0.0"
                }
            }
        };

        var json = JsonSerializer.Serialize(runtimeConfig, new JsonSerializerOptions
        {
            WriteIndented = true
        });
        File.WriteAllText(runtimeConfigPath, json);
    }

    private static IEnumerable<string> SplitLines(string text)
    {
        return text
            .Replace("\r\n", "\n")
            .Replace("\r", "\n")
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);
    }

    private static bool IsBuildShortcut(KeyEvent keyEvent)
    {
        var key = keyEvent.Key;
        return key == (Key.CtrlMask | Key.ShiftMask | Key.B)
               || key == (Key.CtrlMask | Key.B)
               || key == (Key.AltMask | Key.B)
               || key == (Key.CtrlMask | Key.F5)
               || key == Key.F7;
    }

    private static bool IsRunShortcut(KeyEvent keyEvent)
    {
        var key = keyEvent.Key;
        return key == Key.F5;
    }

    private static bool IsFormatShortcut(KeyEvent keyEvent)
    {
        var key = keyEvent.Key;
        return key == (Key.CtrlMask | Key.ShiftMask | Key.F)
               || key == (Key.CtrlMask | Key.F)
               || key == (Key.AltMask | Key.F)
               || key == Key.F8;
    }

    private static bool IsManualCompletionShortcut(KeyEvent keyEvent)
    {
        var key = keyEvent.Key;
        return key == (Key.CtrlMask | Key.Space);
    }

    private static bool IsCompletionTriggerKey(KeyEvent keyEvent)
    {
        if (keyEvent.IsCtrl || keyEvent.IsAlt)
            return false;

        return keyEvent.KeyValue is '.' or ':';
    }

    private static void SaveDocument(string filePath, CodeTextView editor)
    {
        if (string.IsNullOrEmpty(filePath))
        {
            SetStatus("Save failed (no file path)");
            return;
        }

        try
        {
            File.WriteAllText(filePath, editor.Text?.ToString() ?? string.Empty);
            SetStatus("Saved");
        }
        catch
        {
            SetStatus("Save failed");
        }
    }

    private static void FormatDocument(CodeTextView editor)
    {
        _ = FormatDocumentAsync(editor);
    }

    private static async Task FormatDocumentAsync(CodeTextView editor)
    {
        try
        {
            await InvokeOnMainLoopAsync(() => SetStatus("Formatting...")).ConfigureAwait(false);
            var text = await InvokeOnMainLoopAsync(() => editor.Text?.ToString() ?? string.Empty).ConfigureAwait(false);

            var tree = SyntaxTree.ParseText(text);
            var normalizer = new SyntaxNormalizer();
            var formattedRoot = normalizer.Visit(tree.GetRoot());
            var formatted = formattedRoot.ToFullString();

            await InvokeOnMainLoopAsync(() =>
            {
                editor.Text = formatted;
                SetStatus("Ready");
            }).ConfigureAwait(false);
        }
        catch
        {
            await InvokeOnMainLoopAsync(() => SetStatus("Format failed")).ConfigureAwait(false);
        }
    }

    private static void SetStatus(string? status)
    {
        if (_mainWindow is null)
            return;

        var suffix = string.IsNullOrWhiteSpace(status) ? string.Empty : $" [{status}]";
        _mainWindow.Title = _baseWindowTitle + suffix;
        _mainWindow.SetNeedsDisplay();
    }

    private static Task InvokeOnMainLoopAsync(Action action)
    {
        if (Application.MainLoop is null)
        {
            action();
            return Task.CompletedTask;
        }

        var tcs = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        Application.MainLoop.Invoke(() =>
        {
            try
            {
                action();
                tcs.SetResult();
            }
            catch (Exception ex)
            {
                tcs.SetException(ex);
            }
        });

        return tcs.Task;
    }

    private static Task<T> InvokeOnMainLoopAsync<T>(Func<T> action)
    {
        if (Application.MainLoop is null)
            return Task.FromResult(action());

        var tcs = new TaskCompletionSource<T>(TaskCreationOptions.RunContinuationsAsynchronously);
        Application.MainLoop.Invoke(() =>
        {
            try
            {
                tcs.SetResult(action());
            }
            catch (Exception ex)
            {
                tcs.SetException(ex);
            }
        });

        return tcs.Task;
    }
}
