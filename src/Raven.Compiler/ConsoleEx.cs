using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
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
        AnsiConsole.MarkupLine($"Build [red]failed with {result.Diagnostics.Where(x => x.Severity == DiagnosticSeverity.Error).Count()} error(s)[/]");
    }

    public static void Failed(int errorsCount)
    {
        AnsiConsole.MarkupLine($"Build [red]failed with {errorsCount} error(s)[/]");
    }

    public static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics) =>
        PrintDiagnostics(diagnostics, compilation: null, highlightDiagnostics: false, includeSuggestions: false);

    public static void PrintDiagnostics(
        IEnumerable<Diagnostic> diagnostics,
        Compilation? compilation,
        bool highlightDiagnostics,
        bool includeSuggestions)
    {
        var diagnosticArray = diagnostics.ToArray();

        if (!highlightDiagnostics || compilation is null)
        {
            PrintDiagnosticList(diagnosticArray, includeSuggestions);
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
                        diagnostics: group, includeSuggestions: includeSuggestions);

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

            PrintDiagnosticList(fallback, includeSuggestions);
        }
    }

    private static void PrintDiagnosticList(IEnumerable<Diagnostic> diagnostics, bool includeSuggestions)
    {
        foreach (var diagnostic in diagnostics)
        {
            var descriptor = diagnostic.Descriptor;
            var lineSpan = diagnostic.Location.GetLineSpan();
            var path = lineSpan.Path;

            string? fileDirectory = null;
            string fileName = "";

            if (!string.IsNullOrEmpty(path))
            {
                fileDirectory = Path.GetDirectoryName(path);
                fileName = Path.GetFileName(path);

#if DEBUG
                if (fileDirectory is not null)
                    fileDirectory = Path.GetRelativePath(Environment.CurrentDirectory, fileDirectory);
#endif
            }

            if (!string.IsNullOrEmpty(fileDirectory))
            {
                fileDirectory += "/";
            }

            fileName = string.IsNullOrEmpty(fileName) ? "<unknown>" : fileName;

            var fileLocation = $"({lineSpan.StartLinePosition.Line + 1},{lineSpan.StartLinePosition.Character + 1})";

            var color = diagnostic.Severity switch
            {
                DiagnosticSeverity.Warning => ConsoleColor.Green,
                DiagnosticSeverity.Error => ConsoleColor.Red,
                _ => ConsoleColor.Black
            };

            AnsiConsole.MarkupLine($"{fileDirectory}[bold]{fileName}[/]{fileLocation}: [bold {color}]{diagnostic.Severity.ToString().ToLower()} {descriptor.Id}[/]: {Markup.Escape(diagnostic.GetMessage())}");

            if (includeSuggestions &&
                EducationalDiagnosticProperties.TryGetRewriteSuggestion(diagnostic, out var originalCode, out var rewrittenCode))
            {
                AnsiConsole.MarkupLine("  [grey]You wrote:[/]");
                AnsiConsole.MarkupLine($"    [red]{Markup.Escape(originalCode)}[/]");
                AnsiConsole.MarkupLine("  [grey]Write this instead:[/]");
                AnsiConsole.MarkupLine($"    [green]{Markup.Escape(rewrittenCode)}[/]");
            }
        }
    }
}
