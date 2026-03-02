using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MergeStringLiteralConcatenationAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void StringLiteralChain_ReportsDiagnostic()
    {
        const string code = """
func message() -> string {
    return "Hello" + ", " + "world"
}
""";

        var verifier = CreateAnalyzerVerifier<MergeStringLiteralConcatenationAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(MergeStringLiteralConcatenationAnalyzer.DiagnosticId).WithLocation(2, 27)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MixedConcatenation_DoesNotReport()
    {
        const string code = """
func greet(name: string) -> string {
    return "Hello, " + name + "!"
}
""";

        var verifier = CreateAnalyzerVerifier<MergeStringLiteralConcatenationAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
