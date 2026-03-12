using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MissingReturnDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void NonUnitFunction_WithEmptyBody_ReportsMissingReturn()
    {
        var code = """
func Main() -> int {
}
""";

        CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NotAllCodePathsReturnAValue.Id).WithSpan(1, 6, 1, 10)
            ]).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithReachableEndPoint_ReportsMissingReturn()
    {
        var code = """
func Main() -> int {
    if true {
        return 1
    }
}
""";

        CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NotAllCodePathsReturnAValue.Id).WithSpan(1, 6, 1, 10)
            ]).Verify();
    }

    [Fact]
    public void UnitFunction_WithEmptyBody_DoesNotReportMissingReturn()
    {
        var code = """
func Main() -> () {
}
""";

        CreateVerifier(code).Verify();
    }
}
