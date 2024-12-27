using Raven.CodeAnalysis;

using static Raven.ConsoleEx;

namespace Raven;

static class EmitResultEx
{
    public static void Print(this EmitResult result)
    {
        // Check the result
        if (!result.Success)
        {
            PrintDiagnostics(result.Diagnostics);

            Console.WriteLine();

            Failed(result);
        }
        else
        {
            var warningsCount = result.Diagnostics
                .Count(x => x.Descriptor.DefaultSeverity == DiagnosticSeverity.Warning);

            if (warningsCount > 0)
            {
                SucceededWithWarnings(warningsCount);
            }
            else
            {
                Succeeded();
            }
        }
    }
}