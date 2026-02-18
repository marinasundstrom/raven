using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class EventDelegateMustBeNullableAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void NonNullableEventDelegate_ReportsDiagnostic()
    {
        const string code = """
class C {
    public event Clicked: System.Action;
}
""";

        var verifier = CreateAnalyzerVerifier<EventDelegateMustBeNullableAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(EventDelegateMustBeNullableAnalyzer.DiagnosticId)
                    .WithSpan(2, 27, 2, 40)
                    .WithArguments("Clicked", "() -> ()", "() -> ()?")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void NullableEventDelegate_NoDiagnostic()
    {
        const string code = """
class C {
    public event Clicked: System.Action?;
}
""";

        var verifier = CreateAnalyzerVerifier<EventDelegateMustBeNullableAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
