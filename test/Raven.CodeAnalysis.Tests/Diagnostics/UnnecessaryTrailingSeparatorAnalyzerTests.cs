using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnnecessaryTrailingSeparatorAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ParameterList_WithTrailingComma_ReportsDiagnostic()
    {
        const string code = "class C { func M(x: int, y: int,) -> () {} }";

        var verifier = CreateAnalyzerVerifier<UnnecessaryTrailingSeparatorAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnnecessaryTrailingSeparatorAnalyzer.DiagnosticId)
                    .WithSpan(1, 32, 1, 33)
                    .WithArguments(",")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void TypeArgumentList_WithTrailingComma_ReportsDiagnostic()
    {
        const string code = """
class Box<T> {}

class C {
    func M() -> () {
        let value = Box<int,>()
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UnnecessaryTrailingSeparatorAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnnecessaryTrailingSeparatorAnalyzer.DiagnosticId)
                    .WithSpan(5, 28, 5, 29)
                    .WithArguments(",")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void NewlineSeparatedList_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    func M(
        x: int
        y: int
    ) -> () {}
}
""";

        var verifier = CreateAnalyzerVerifier<UnnecessaryTrailingSeparatorAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void EnumMemberList_WithTrailingComma_DoesNotReportDiagnostic()
    {
        const string code = "enum E { A, B, }";

        var verifier = CreateAnalyzerVerifier<UnnecessaryTrailingSeparatorAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
