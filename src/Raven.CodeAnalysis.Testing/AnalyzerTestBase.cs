using System.Collections.Immutable;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis.Testing;

public abstract class AnalyzerTestBase
{
    protected AnalyzerVerifier<TAnalyzer> CreateAnalyzerVerifier<TAnalyzer>(
        string testCode,
        IEnumerable<DiagnosticResult>? expectedDiagnostics = null,
        IEnumerable<string>? disabledDiagnostics = null,
        bool enableSuggestions = false,
        IDictionary<string, ReportDiagnostic>? specificDiagnosticOptions = null,
        ReturnedValueHandlingMode? returnedValueHandlingMode = null,
        IEnumerable<MetadataReference>? additionalReferences = null,
        FrameworkProjectionMode? frameworkProjectionMode = null)
        where TAnalyzer : DiagnosticAnalyzer, new()
    {
        return new AnalyzerVerifier<TAnalyzer>
        {
            Test = new Test
            {
                TestCode = testCode,
                ExpectedDiagnostics = expectedDiagnostics?.ToList() ?? [],
                DisabledDiagnostics = disabledDiagnostics?.ToList() ?? [],
                State = new TestState
                {
                    AdditionalReferences = additionalReferences?.ToImmutableArray() ?? [],
                    EnableSuggestions = enableSuggestions,
                    FrameworkProjectionMode = frameworkProjectionMode,
                    ReturnedValueHandlingMode = returnedValueHandlingMode,
                    SpecificDiagnosticOptions = specificDiagnosticOptions is not null
                        ? new Dictionary<string, ReportDiagnostic>(specificDiagnosticOptions, StringComparer.OrdinalIgnoreCase)
                        : []
                }
            }
        };
    }
}
