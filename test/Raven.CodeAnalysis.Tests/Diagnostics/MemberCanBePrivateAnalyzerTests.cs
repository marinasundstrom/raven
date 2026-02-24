using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MemberCanBePrivateAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void PublicMethod_OnlyUsedInsideType_ReportsDiagnostic()
    {
        const string code = """
class Counter {
    public func Increment() -> () { }

    func Run() -> () {
        Increment()
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBePrivateAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(MemberCanBePrivateAnalyzer.DiagnosticId)
                    .WithLocation(2, 17)
                    .WithArguments("Increment")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethod_UsedOutsideType_DoesNotReport()
    {
        const string code = """
val counter = Counter()
counter.Increment()

class Counter {
    public func Increment() -> () { }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBePrivateAnalyzer>(
            code,
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                "RAV1011"
            ]);

        verifier.Verify();
    }
}
