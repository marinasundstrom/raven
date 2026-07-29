using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class ImmutableCollectionOperationResultAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ImmutableListMutationLikeCall_ReportsDiagnosticWhenIgnored()
    {
        const string code = """
import System.Collections.Immutable.*

func Test() -> () {
    let list = [1, 2, 3]
    list.Add(4)
}
""";

        var verifier = CreateAnalyzerVerifier<ImmutableCollectionOperationResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ImmutableCollectionOperationResultAnalyzer.DiagnosticId)
                    .WithSpan(5, 5, 5, 16)
                    .WithArguments("Add")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableArrayMutationLikeCall_ReportsDiagnosticWhenIgnored()
    {
        const string code = """
import System.Collections.Immutable.*

func Test() -> () {
    let values: ImmutableArray<int> = [1, 2, 3]
    values.Add(4)
}
""";

        var verifier = CreateAnalyzerVerifier<ImmutableCollectionOperationResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ImmutableCollectionOperationResultAnalyzer.DiagnosticId)
                    .WithSpan(5, 5, 5, 18)
                    .WithArguments("Add")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableListMutationLikeCall_DoesNotReportWhenCaptured()
    {
        const string code = """
func Test() -> () {
    let list = [1, 2, 3]
    let newList = list.Add(4)
}
""";

        var verifier = CreateAnalyzerVerifier<ImmutableCollectionOperationResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableListQueryCall_DoesNotReportWhenReturnTypeIsNotImmutableCollection()
    {
        const string code = """
func Test() -> () {
    let list = [1, 2, 3]
    list.Contains(2)
}
""";

        var verifier = CreateAnalyzerVerifier<ImmutableCollectionOperationResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void TrailingImplicitReturn_DoesNotReport()
    {
        const string code = """
func Test() -> System.Collections.Immutable.ImmutableList<int> {
    let list = [1, 2, 3]
    list.Add(4)
}
""";

        var verifier = CreateAnalyzerVerifier<ImmutableCollectionOperationResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
