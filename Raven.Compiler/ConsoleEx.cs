using Raven.CodeAnalysis;

using Spectre.Console;

namespace Raven;

static class ConsoleEx
{
    public static void SucceededWithWarnings(int warningsCount)
    {
        AnsiConsole.MarkupLine($"Build [yellow]succeeded with {warningsCount} warning(s)[/]");
    }

    public static void Succeeded()
    {
        AnsiConsole.MarkupLine($"Build [green]succeeded[/]");
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

            AnsiConsole.MarkupLine($"{fileDirectory}[bold]{fileName}[/]{fileLocation}: [bold {color}]{descriptor.DefaultSeverity.ToString().ToLower()} {descriptor.Id}[/]: {string.Format(descriptor.MessageFormat, diagnostic.GetMessageArgs() ?? [])}");
        }
    }
}
