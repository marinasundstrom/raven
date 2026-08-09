using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Scripting;

namespace Raven.CommandLine;

internal static class ScriptCommand
{
    internal static async Task<int> ExecuteEvalAsync(string[] args)
    {
        if (args.Length != 2)
        {
            Console.Error.WriteLine("Usage: rvn eval <code>");
            return 1;
        }

        try
        {
            using var state = await RavenScript.RunAsync(args[1]).ConfigureAwait(false);
            WriteResult(state);
            return 0;
        }
        catch (RavenCompilationException exception)
        {
            WriteDiagnostics(exception.Diagnostics);
            return 1;
        }
        catch (Exception exception)
        {
            Console.Error.WriteLine(exception.Message);
            return 1;
        }
    }

    internal static void WriteResult(ScriptState state)
    {
        if (!state.HasReturnValue)
            return;

        Console.WriteLine(state.ReturnValue?.ToString() ?? "null");
    }

    internal static void WriteDiagnostics(IEnumerable<Diagnostic> diagnostics)
    {
        foreach (var diagnostic in diagnostics.Where(static diagnostic =>
                     diagnostic.Severity is DiagnosticSeverity.Error or DiagnosticSeverity.Warning))
        {
            Console.Error.WriteLine(diagnostic.GetDescription());
        }
    }
}
