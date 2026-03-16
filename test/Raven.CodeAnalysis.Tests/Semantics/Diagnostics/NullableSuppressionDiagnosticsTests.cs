using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullableSuppressionDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void NullableReferenceSuppression_ReportsWarning()
    {
        var code = """
func Foo(name: string?) -> int {
    val required = name!
    return required.Length
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NullableSuppressionUsed.Id)
                    .WithSpan(2, 24, 2, 25)
                    .WithSeverity(DiagnosticSeverity.Warning)
            ],
            disabledDiagnostics: ["RAV9012"]);

        verifier.Verify();
    }

    [Fact]
    public void NullableValueSuppression_ReportsWarning()
    {
        var code = """
func Foo(value: int?) -> int {
    val required = value!
    return required + 1
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NullableSuppressionUsed.Id)
                    .WithSpan(2, 25, 2, 26)
                    .WithSeverity(DiagnosticSeverity.Warning)
            ],
            disabledDiagnostics: ["RAV9012"]);

        verifier.Verify();
    }

    [Fact]
    public void NonNullableSuppression_DoesNotReportWarning()
    {
        var code = """
func Foo(value: string) -> int {
    val required = value!
    return required.Length
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }
}
