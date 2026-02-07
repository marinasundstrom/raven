using System.Reflection;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis;

internal static class AnalyzerDiagnosticIdValidator
{
    private const string RavenDiagnosticPrefix = "RAV";
    private static readonly Assembly RavenCodeAnalysisAssembly = typeof(CompilerDiagnostics).Assembly;

    public static bool IsInternalAnalyzer(DiagnosticAnalyzer analyzer)
    {
        ArgumentNullException.ThrowIfNull(analyzer);
        return analyzer.GetType().Assembly == RavenCodeAnalysisAssembly;
    }

    public static void Validate(DiagnosticAnalyzer analyzer, Diagnostic diagnostic, bool isInternalAnalyzer)
    {
        ArgumentNullException.ThrowIfNull(analyzer);
        ArgumentNullException.ThrowIfNull(diagnostic);

        Validate(analyzer.GetType().FullName ?? analyzer.GetType().Name, diagnostic.Descriptor.Id, isInternalAnalyzer);
    }

    internal static void Validate(string analyzerName, string diagnosticId, bool isInternalAnalyzer)
    {
        if (string.IsNullOrEmpty(diagnosticId))
            return;

        if (!isInternalAnalyzer && diagnosticId.StartsWith(RavenDiagnosticPrefix, StringComparison.Ordinal))
        {
            throw new InvalidOperationException(
                $"External analyzer '{analyzerName}' cannot report diagnostics with the reserved '{RavenDiagnosticPrefix}' prefix ('{diagnosticId}').");
        }

        if (isInternalAnalyzer && CompilerDiagnostics.IsDiagnosticDefined(diagnosticId))
        {
            throw new InvalidOperationException(
                $"Internal analyzer '{analyzerName}' cannot report diagnostic '{diagnosticId}' because it is already defined by Raven compiler diagnostics.");
        }
    }
}
