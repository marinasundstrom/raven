using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class YieldStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void Iterator_YieldValueNotAssignableToElementType_ReportsDiagnostic()
    {
        var code = """
import System.Collections.Generic.*

func Values() -> IEnumerable<int> {
    yield return 0.5
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(4, 18, 4, 21)
                    .WithArguments("double", "int"),
                new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id)
                    .WithSpan(4, 18, 4, 21)
                    .WithArguments("double", "int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void AsyncIterator_YieldExpressionNotAssignableToElementType_ReportsDiagnostic()
    {
        var code = """
import System.Collections.Generic.*
import System.Threading.Tasks.*

async func Values() -> IAsyncEnumerable<int> {
    await Task.Delay(0)
    yield 1
    match true {
        _ => yield 0.5
    }
    yield break
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(8, 20, 8, 23)
                    .WithArguments("double", "int"),
                new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id)
                    .WithSpan(8, 20, 8, 23)
                    .WithArguments("double", "int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void AsyncIterator_RangeLoopYieldNotAssignableToElementType_ReportsDiagnostic()
    {
        var code = """
import System.Collections.Generic.*
import System.Threading.Tasks.*

async func Values() -> IAsyncEnumerable<int> {
    for i in 1..5.0 by 0.1 {
        yield i
        await Task.Delay(0)
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(6, 15, 6, 16)
                    .WithArguments("double", "int"),
                new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id)
                    .WithSpan(6, 15, 6, 16)
                    .WithArguments("double", "int")
            ]);

        verifier.Verify();
    }
}
