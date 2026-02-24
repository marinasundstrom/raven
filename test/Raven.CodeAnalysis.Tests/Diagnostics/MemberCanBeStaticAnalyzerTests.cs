using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MemberCanBeStaticAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void MethodWithoutInstanceAccess_ReportsDiagnostic()
    {
        const string code = """
class MathOps {
    func Add(x: int, y: int) -> int => x + y
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(MemberCanBeStaticAnalyzer.DiagnosticId)
                    .WithLocation(2, 10)
                    .WithArguments("Add")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodUsingInstanceField_DoesNotReport()
    {
        const string code = """
class Counter {
    private field _count: int = 0

    func Increment() -> () {
        _count += 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
