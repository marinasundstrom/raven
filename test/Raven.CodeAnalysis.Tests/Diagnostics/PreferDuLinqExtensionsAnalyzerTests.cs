using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferDuLinqExtensionsAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void First_ReportsPreferenceForFirstOrError()
    {
        const string code = """
import System.Linq.*

func Test() {
    val arr: int[] = [1, 2, 3]
    val first = arr.First()
}
""";

        var verifier = CreateAnalyzerVerifier<PreferDuLinqExtensionsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferDuLinqExtensionsAnalyzer.DiagnosticId)
                    .WithSpan(5, 21, 5, 26)
                    .WithArguments("FirstOrError")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void FirstOrDefault_ReportsPreferenceForFirstOrNone()
    {
        const string code = """
import System.Linq.*

func Test() {
    val arr: int[] = [1, 2, 3]
    val maybeFirst = arr.FirstOrDefault()
}
""";

        var verifier = CreateAnalyzerVerifier<PreferDuLinqExtensionsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferDuLinqExtensionsAnalyzer.DiagnosticId)
                    .WithSpan(5, 26, 5, 40)
                    .WithArguments("FirstOrNone")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void AlreadyUsingDuExtensions_NoDiagnostic()
    {
        const string code = """
import System.Linq.*

func Test() {
    val arr: int[] = [1, 2, 3]
    val first = arr.FirstOrError(() => "no items")
    val maybe = arr.FirstOrNone()
}
""";

        var verifier = CreateAnalyzerVerifier<PreferDuLinqExtensionsAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id,
                CompilerDiagnostics.MemberDoesNotContainDefinition.Id
            ]);

        verifier.Verify();
    }
}
