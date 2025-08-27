# Analyzer verifier

The analyzer testing framework builds Raven projects using `RavenWorkspace` so analyzers run
with full project context. `AnalyzerVerifier` executes an analyzer against the provided
source code and collects diagnostics for verification.

This example checks that the `MissingReturnTypeAnnotationAnalyzer` reports a diagnostic when a
function omits its return type:

```csharp
public class ReturnTypeTests : AnalyzerTestBase<MissingReturnTypeAnnotationAnalyzer>
{
    [Fact]
    public async Task Reports_missing_return_type()
    {
        const string source = """
        def foo() => 42
        """;

        var verifier = CreateAnalyzerVerifier(source);

        var result = await verifier.GetResultAsync();

        result.MatchedDiagnostics.ShouldContain(d => d.Id == "RAV1003");
    }
}
```

`AnalyzerTestBase<TAnalyzer>` wires up the workspace and exposes a `CreateAnalyzerVerifier`
helper that simplifies test setup. The verifier also supports specifying expected and disabled
diagnostics similar to `DiagnosticVerifier`.
