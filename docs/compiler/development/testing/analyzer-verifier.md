# Analyzer verifier

`AnalyzerVerifier<TAnalyzer>` runs analyzers through `RavenWorkspace` so diagnostics are evaluated with project-level context.

For the analyzer authoring model and API surface, see
[Analyzers](../../analyzers/README.md).

Derive from `AnalyzerTestBase` and call `CreateAnalyzerVerifier<TAnalyzer>(...)` to reduce setup.

```csharp
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class ReturnTypeTests : AnalyzerTestBase
{
    [Fact]
    public void Reports_missing_return_type()
    {
        const string source = """
func foo() {
    return 42
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(
            source,
            [new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
```

For code-fix scenarios, use the [Code fix verifier](code-fix-verifier.md).

Analyzer verifier state can also model compiler options that affect analyzer participation.
For example, the returned-value analyzer is off by default, so tests that expect `RAV9029`
must pass `returnedValueHandlingMode: ReturnedValueHandlingMode.Full` to
`CreateAnalyzerVerifier`. Use `specificDiagnosticOptions` only for severity remapping or
suppression; it should not be used to turn project-mode analyzers on.
