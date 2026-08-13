using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class PreferLoopOverWhileTrueAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void WhileTrue_ReportsDiagnostic()
    {
        const string code = "while true { break }";

        var verifier = CreateAnalyzerVerifier<PreferLoopOverWhileTrueAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferLoopOverWhileTrueAnalyzer.DiagnosticId)
                    .WithLocation(1, 1)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ParenthesizedTrue_ReportsDiagnostic()
    {
        const string code = "while ((true)) { break }";

        var verifier = CreateAnalyzerVerifier<PreferLoopOverWhileTrueAnalyzer>(
            code,
            expectedDiagnostics: [new DiagnosticResult(PreferLoopOverWhileTrueAnalyzer.DiagnosticId).WithAnySpan()],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Theory]
    [InlineData("while false { }")]
    [InlineData("let condition = true\nwhile condition { }")]
    [InlineData("loop { break }")]
    public void OtherLoops_DoNotReportDiagnostic(string code)
    {
        var verifier = CreateAnalyzerVerifier<PreferLoopOverWhileTrueAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
