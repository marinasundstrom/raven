using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnsafeUnwrapAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void PartialOptionAndResultExtraction_ReportsDiagnosticOnMethodName()
    {
        const string code = """
import System.*

func Test(result: Result<int, string>, option: Option<int>) {
    let value = result.UnwrapOrThrow()
    let error = result.UnwrapError()
    let optionalValue = option.UnwrapOrThrow()
}
""";

        var verifier = CreateAnalyzerVerifier<UnsafeUnwrapAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnsafeUnwrapAnalyzer.DiagnosticId)
                    .WithSpan(4, 24, 4, 37)
                    .WithArguments("UnwrapOrThrow"),
                new DiagnosticResult(UnsafeUnwrapAnalyzer.DiagnosticId)
                    .WithSpan(5, 24, 5, 35)
                    .WithArguments("UnwrapError"),
                new DiagnosticResult(UnsafeUnwrapAnalyzer.DiagnosticId)
                    .WithSpan(6, 32, 6, 45)
                    .WithArguments("UnwrapOrThrow"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            additionalReferences: [TestMetadataReferences.RavenCore]);

        verifier.Verify();
    }

    [Fact]
    public void SafeFallbackExtraction_DoesNotReportDiagnostic()
    {
        const string code = """
import System.*

func Test(result: Result<int, string>, option: Option<int>) {
    let a = result.UnwrapOr(0)
    let b = result.UnwrapOrDefault()
    let c = result.UnwrapOrElse(() => 0)
    let d = option.UnwrapOr(0)
    let e = option.UnwrapOrDefault()
    let f = option.UnwrapOrElse(() => 0)
}
""";

        var verifier = CreateAnalyzerVerifier<UnsafeUnwrapAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            additionalReferences: [TestMetadataReferences.RavenCore]);

        verifier.Verify();
    }

    [Fact]
    public void UnwrapAndExpectExtensions_ReportsDiagnostic()
    {
        const string code = """
import System.*

extension UnsafeResultExtensions<T, E> for Result<T, E> {
    func Unwrap() -> T => default
    func Expect(message: string) -> T => default
}

func Test(result: Result<int, string>) {
    let value = result.Unwrap()
    let expected = result.Expect("value")
}
""";

        var verifier = CreateAnalyzerVerifier<UnsafeUnwrapAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnsafeUnwrapAnalyzer.DiagnosticId)
                    .WithSpan(9, 24, 9, 30)
                    .WithArguments("Unwrap"),
                new DiagnosticResult(UnsafeUnwrapAnalyzer.DiagnosticId)
                    .WithSpan(10, 27, 10, 33)
                    .WithArguments("Expect"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            additionalReferences: [TestMetadataReferences.RavenCore]);

        verifier.Verify();
    }

    [Fact]
    public void SameMethodNameOnUnrelatedType_DoesNotReportDiagnostic()
    {
        const string code = """
class Box {
    func UnwrapOrThrow() -> int => 42
    func Expect(message: string) -> int => 42
}

func Test(box: Box) {
    let value = box.UnwrapOrThrow()
    let expected = box.Expect("value")
}
""";

        var verifier = CreateAnalyzerVerifier<UnsafeUnwrapAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
