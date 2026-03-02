using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class StringConcatenationAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void MixedStringConcatenation_ReportsDiagnostic()
    {
        const string code = """
func greet(name: string) -> string {
    return "Hello, " + name + "!"
}
""";

        var verifier = CreateAnalyzerVerifier<StringConcatenationAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(StringConcatenationAnalyzer.DiagnosticId).WithLocation(2, 29)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void LiteralOnlyConcatenation_DoesNotReport()
    {
        const string code = """
func message() -> string {
    return "Hello" + ", " + "world"
}
""";

        var verifier = CreateAnalyzerVerifier<StringConcatenationAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
