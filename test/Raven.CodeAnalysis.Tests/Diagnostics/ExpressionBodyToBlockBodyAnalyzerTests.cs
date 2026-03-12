using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class ExpressionBodyToBlockBodyAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void Method_WithExpressionBody_ReportsDiagnostic()
    {
        const string code = """
class C {
    func Get() -> int => 42
}
""";

        var verifier = CreateAnalyzerVerifier<ExpressionBodyToBlockBodyAnalyzer>(
            code,
            [
                new DiagnosticResult(ExpressionBodyToBlockBodyAnalyzer.DiagnosticId)
                    .WithSeverity(DiagnosticSeverity.Info)
                    .WithLocation(2, 10)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void Method_WithBlockBody_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    func Get() -> int {
        return 42
    }
}
""";

        var verifier = CreateAnalyzerVerifier<ExpressionBodyToBlockBodyAnalyzer>(
            code,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
