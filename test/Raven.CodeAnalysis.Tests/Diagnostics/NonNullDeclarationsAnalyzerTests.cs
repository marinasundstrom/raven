using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class NonNullDeclarationsAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void NullableLocalType_ReportsDiagnosticWithReplacementSuggestion()
    {
        const string code = """
func Test() {
    var value: int? = null
}
""";

        var verifier = CreateAnalyzerVerifier<NonNullDeclarationsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId)
                    .WithSpan(2, 16, 2, 20)
                    .WithArguments("Option<int>", "int?")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void NullablePropertyType_ReportsDiagnosticWithReplacementSuggestion()
    {
        const string code = """
class C {
    val value: int? = null
}
""";

        var verifier = CreateAnalyzerVerifier<NonNullDeclarationsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId)
                    .WithSpan(2, 16, 2, 20)
                    .WithArguments("Option<int>", "int?")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void NullableEventHandlerType_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    event Clicked: System.Action?
}
""";

        var verifier = CreateAnalyzerVerifier<NonNullDeclarationsAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);
        verifier.Verify();
    }
}
