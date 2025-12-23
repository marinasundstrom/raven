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
        PrintDiagnostics(diagnostics, compilation: null);

    public static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics, Compilation? compilation)
    {
        var diagnosticArray = diagnostics.ToArray();

        PrintDiagnosticList(diagnosticArray);
    }

    private static void PrintDiagnosticList(IEnumerable<Diagnostic> diagnostics)
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
        }
    }
}
