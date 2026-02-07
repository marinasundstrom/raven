using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class VarCanBeValAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void VarLocal_NotReassigned_ReportsDiagnostic()
    {
        const string code = """
class C {
    public M() -> unit {
        var count = 0
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeValAnalyzer.DiagnosticId)
                    .WithLocation(3, 13)
                    .WithArguments("count")
            ],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_Reassigned_NoDiagnostic()
    {
        const string code = """
class C {
    public M() -> unit {
        var count = 0
        count = 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }
}
