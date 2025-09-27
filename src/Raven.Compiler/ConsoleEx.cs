using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Spectre.Console;

namespace Raven;

static class ConsoleEx
{
    public static void SucceededWithWarnings(int warningsCount, TimeSpan elapsed)
    {
        AnsiConsole.MarkupLine($"Build [bold yellow]succeeded with {warningsCount} warning(s)[/] in {elapsed.TotalSeconds:F1}s");
    }

    public static void Succeeded(TimeSpan elapsed)
    {
        AnsiConsole.MarkupLine($"Build [bold green]succeeded[/] in {elapsed.TotalSeconds:F1}s");
    }

    public static void Failed(EmitResult result)
    {
        AnsiConsole.MarkupLine($"Build [red]failed with {result.Diagnostics.Count()} error(s)[/]");
    }

    public static void Failed(int errorsCount)
    {
        AnsiConsole.MarkupLine($"Build [red]failed with {errorsCount} error(s)[/]");
    }

    public static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics) =>
        PrintDiagnostics(diagnostics, compilation: null, highlightDiagnostics: false);

    public static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics, Compilation? compilation, bool highlightDiagnostics)
    {
        var diagnosticArray = diagnostics.ToArray();

        if (!highlightDiagnostics || compilation is null)
        {
            PrintDiagnosticList(diagnosticArray);
            return;
        }

        var highlightable = new List<Diagnostic>();
        var fallback = new List<Diagnostic>();

        foreach (var diagnostic in diagnosticArray)
        {
            if (diagnostic.Location.SourceTree is null)
            {
                fallback.Add(diagnostic);
                continue;
            }

            highlightable.Add(diagnostic);
        }

        var highlightedSections = new List<string>();

        if (highlightable.Count > 0)
        {
            var previousScheme = ConsoleSyntaxHighlighter.ColorScheme;
            try
            {
                ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

                foreach (var group in highlightable
                             .GroupBy(d => d.Location.SourceTree!)
                             .OrderBy(g => g.Key.FilePath, StringComparer.Ordinal)
                             .ThenBy(g => g.Min(d => d.Location.SourceSpan.Start)))
                {
                    var tree = group.Key;
                    var root = tree.GetRoot();
                    var text = root.WriteNodeToText(compilation, includeDiagnostics: true, diagnosticsOnly: true,
                        diagnostics: group);

                    if (string.IsNullOrWhiteSpace(text))
                    {
                        fallback.AddRange(group);
                        continue;
                    }

                    highlightedSections.Add(text.TrimEnd());
                }
            }
            finally
            {
                ConsoleSyntaxHighlighter.ColorScheme = previousScheme;
            }
        }

        if (highlightedSections.Count > 0)
            Console.WriteLine(string.Join(Environment.NewLine + Environment.NewLine, highlightedSections));
        else
            fallback.AddRange(highlightable);

        if (fallback.Count > 0)
        {
            if (highlightedSections.Count > 0)
                Console.WriteLine();

            PrintDiagnosticList(fallback);
        }
    }

    private static void PrintDiagnosticList(IEnumerable<Diagnostic> diagnostics)
    {
        foreach (var diagnostic in diagnostics)
        {
            var descriptor = diagnostic.Descriptor;
            var location = diagnostic.Location.GetLineSpan();

            var fileDirectory = Path.GetDirectoryName(location.Path);

#if DEBUG
            fileDirectory = Path.GetRelativePath(Environment.CurrentDirectory, fileDirectory!);
#endif

            if (!string.IsNullOrEmpty(fileDirectory))
            {
                fileDirectory += "/";
            }

            var fileName = Path.GetFileName(location.Path);

            var fileLocation = $"({location.StartLinePosition.Line + 1},{location.StartLinePosition.Character + 1})";

            var color = diagnostic.Severity switch
            {
                DiagnosticSeverity.Warning => ConsoleColor.Green,
                DiagnosticSeverity.Error => ConsoleColor.Red,
                _ => ConsoleColor.Black
            };

            AnsiConsole.MarkupLine($"{fileDirectory}[bold]{fileName}[/]{fileLocation}: [bold {color}]{diagnostic.Severity.ToString().ToLower()} {descriptor.Id}[/]: {Markup.Escape(diagnostic.GetMessage())}");
        }
    }
}
