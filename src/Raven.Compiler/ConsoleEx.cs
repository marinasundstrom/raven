using Raven.CodeAnalysis;

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

    public static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics)
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

            var color = diagnostic.Descriptor.DefaultSeverity switch
            {
                DiagnosticSeverity.Warning => ConsoleColor.Green,
                DiagnosticSeverity.Error => ConsoleColor.Red,
                _ => ConsoleColor.Black
            };

            AnsiConsole.MarkupLine($"{fileDirectory}[bold]{fileName}[/]{fileLocation}: [bold {color}]{descriptor.DefaultSeverity.ToString().ToLower()} {descriptor.Id}[/]: {Markup.Escape(diagnostic.GetMessage())}");
        }
    }
}