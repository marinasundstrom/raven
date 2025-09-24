using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExceptionHandlingTests : DiagnosticTestBase
{
    [Fact]
    public void TryStatement_WithoutCatchOrFinally_ReportsDiagnostic()
    {
        var code = "try { }";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1015").WithSpan(1, 1, 1, 4)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void CatchClause_WithNonExceptionType_ReportsDiagnostic()
    {
        var code = """
try {
}
catch (int ex) {
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1016").WithSpan(3, 8, 3, 11).WithArguments("int")
            ]);

        verifier.Verify();
    }
}
