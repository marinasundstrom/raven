using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class LinqExtensionsTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void FirstLastSingleOrError_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val arr = [1, 2, 3]
val first = arr.FirstOrNone(x => x > 1)
val last = arr.LastOrNone()
val only = arr.Where(x => x == 2).SingleOrError(() => "none", () => "many")
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void UnknownMethod_ProducesExpectedDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val arr = [1, 2, 3]
val _ = arr.DoesNotExist()
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0103")
                    .WithAnySpan()
                    .WithArguments("DoesNotExist")
            ]);

        verifier.Verify();
    }
}
