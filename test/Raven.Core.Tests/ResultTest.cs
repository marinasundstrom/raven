using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class ResultTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void MapErrorAndUnwrapError_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val result: Result<int, string> = .Error("boom")
val hasError = result.HasError
val text = result.MapError(message => message + "!").UnwrapError()
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void MapError_WithWrongLambdaType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val result: Result<int, string> = .Error("boom")
val _ = result.MapError((v: int) => v)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501")
                    .WithAnySpan()
                    .WithArguments("method", "MapError", 1)
            ]);

        verifier.Verify();
    }
}
