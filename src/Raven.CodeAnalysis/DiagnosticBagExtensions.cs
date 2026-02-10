using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

// The methods in this partial class are generated from DiagnosticDescriptors.xml
// using tools/DiagnosticsGenerator.
public static partial class DiagnosticBagExtensions
{
    public static void ReportCallIsAmbiguous(
        this DiagnosticBag diagnostics,
        string methodName,
        ImmutableArray<IMethodSymbol> candidates,
        Location location)
    {
        var printableCandidates = candidates
            .Where(static c => c is not null)
            .Take(2)
            .Select(c => c.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .ToArray();

        string first;
        string second;

        if (printableCandidates.Length >= 2)
        {
            first = printableCandidates[0];
            second = printableCandidates[1];
        }
        else if (printableCandidates.Length == 1)
        {
            first = printableCandidates[0];
            second = printableCandidates[0];
        }
        else
        {
            first = methodName;
            second = methodName;
        }

        diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CallIsAmbiguous, location, first, second));
    }

    public static void ReportObsoleteMember(
        this DiagnosticBag diagnostics,
        object? symbolKind,
        object? memberName,
        object? message,
        Location location,
        DiagnosticSeverity severity)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.ObsoleteMember, location, severity, symbolKind, memberName, message));
}
