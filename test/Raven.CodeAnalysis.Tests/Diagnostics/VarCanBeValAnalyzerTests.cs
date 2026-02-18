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
                    .WithLocation(3, 9)
                    .WithArguments("count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

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
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarGlobal_NotReassigned_ReportsDiagnostic()
    {
        const string code = "var count = 0";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeValAnalyzer.DiagnosticId)
                    .WithLocation(1, 1)
                    .WithArguments("count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarGlobal_Reassigned_NoDiagnostic()
    {
        const string code = """
var count = 0
count = 1
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
