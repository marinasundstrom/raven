using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DefaultParameterTests : DiagnosticTestBase
{
    [Fact]
    public void RequiredParameterAfterOptional_ReportsDiagnostic()
    {
        const string code = """
class C {
    public M(x: int = 1, y: int) {}
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0317").WithSpan(2, 26, 2, 27).WithArguments("y")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void DefaultValueMustBeConstant_ReportsDiagnostic()
    {
        const string code = """
class C {
    public M(x: int = 1 + 2) {}
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0318").WithSpan(2, 23, 2, 28).WithArguments("x")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void DefaultValueMustConvertToParameterType_ReportsDiagnostic()
    {
        const string code = """
class C {
    public M(x: int = 3.14) {}
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0319").WithSpan(2, 23, 2, 27).WithArguments("x", "int")
            ]);

        verifier.Verify();
    }
}
