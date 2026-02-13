using System.Collections.Immutable;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis.Testing;

public abstract class CodeFixTestBase
{
    protected CodeFixVerifier<TAnalyzer, TCodeFixProvider> CreateCodeFixVerifier<TAnalyzer, TCodeFixProvider>(
        string testCode,
        string fixedCode,
        IEnumerable<DiagnosticResult>? expectedDiagnostics = null,
        IEnumerable<string>? disabledDiagnostics = null,
        IEnumerable<MetadataReference>? additionalReferences = null,
        int? expectedAppliedFixCount = 1)
        where TAnalyzer : DiagnosticAnalyzer, new()
        where TCodeFixProvider : CodeFixProvider, new()
    {
        var disabled = disabledDiagnostics?.ToList() ?? [];
        disabled.Add("RAV1011");
        disabled.Add("RAV1014");
        disabled.Add("RAV1503");

        return new CodeFixVerifier<TAnalyzer, TCodeFixProvider>
        {
            Test = new CodeFixTest
            {
                TestCode = testCode,
                FixedCode = fixedCode,
                ExpectedDiagnostics = expectedDiagnostics?.ToList() ?? [],
                DisabledDiagnostics = disabled,
                ExpectedAppliedFixCount = expectedAppliedFixCount,
                State = new TestState
                {
                    ReferenceAssemblies = ReferenceAssemblies.Default,
                    AdditionalReferences = additionalReferences?.ToImmutableArray() ?? [],
                }
            }
        };
    }
}
