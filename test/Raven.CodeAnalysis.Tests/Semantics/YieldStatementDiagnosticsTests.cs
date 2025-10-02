using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class YieldStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void YieldReturnInExpressionContext_ReportsDiagnostic()
    {
        var code = """
func main() {
    let value = {
        yield return 42;
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1908").WithSpan(3, 9, 3, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void YieldBreakInExpressionContext_ReportsDiagnostic()
    {
        var code = """
func main() {
    let value = {
        yield break;
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1909").WithSpan(3, 9, 3, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void YieldReturnInNonIteratorMethod_ReportsDiagnostic()
    {
        var code = """
class C {
    Numbers() -> int {
        yield return 42;
        return 0;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV2602").WithSpan(4, 9, 4, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void YieldBreakInNonIteratorMethod_ReportsDiagnostic()
    {
        var code = """
class C {
    Numbers() -> int {
        yield break;
        return 0;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV2602").WithSpan(4, 9, 4, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void YieldReturnInConstructor_ReportsDiagnostic()
    {
        var code = """
class C {
    public init() {
        yield return 42;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV2603").WithSpan(4, 9, 4, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void YieldBreakInConstructor_ReportsDiagnostic()
    {
        var code = """
class C {
    public init() {
        yield break;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV2603").WithSpan(4, 9, 4, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void YieldBreakInNamedConstructor_ReportsDiagnostic()
    {
        var code = """
class C {
    public init WithName() {
        yield break;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV2603").WithSpan(4, 9, 4, 14)
            ]);

        verifier.Verify();
    }
}
