# Code fix verifier

Use `CodeFixVerifier<TAnalyzer, TCodeFixProvider>` to test diagnostics and the resulting fixed source in one assertion flow.

For convenience, derive from `CodeFixTestBase` and call `CreateCodeFixVerifier(...)`.

```csharp
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class MissingReturnTypeCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void Adds_explicit_return_type()
    {
        const string code = """
func Test() {
    return 1
}
""";

        const string fixedCode = """
func Test() -> int {
    return 1
}
""";

        var verifier = CreateCodeFixVerifier<MissingReturnTypeAnnotationAnalyzer, MissingReturnTypeAnnotationCodeFixProvider>(
            code,
            fixedCode,
            expectedDiagnostics:
            [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithAnySpan()
            ]);

        verifier.Verify();
    }
}
```

`CodeFixVerifier` supports:

- `ExpectedDiagnostics`: diagnostics that must be present before applying fixes.
- `DisabledDiagnostics`: diagnostics to ignore during verification.
- `FixedCode`: exact expected source after the fix pipeline runs.
- `ExpectedAppliedFixCount`: expected number of applied fixes.
- `MaxIterations`: maximum apply/reanalyze iterations (default `100`).
- `Test.State.ReferenceAssemblies` and `Test.State.AdditionalReferences`: metadata references used for analysis.

`Verify()` throws `CodeFixVerificationException` with diagnostic mismatch details, expected vs actual fixed code, and applied-fix count differences.
