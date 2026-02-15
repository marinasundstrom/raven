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
    public void MaterializationOrExceptionExtensions_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val arr = [1, 2, 3]
val arrResult = arr.ToArrayOrException()
val listResult = arr.ToListOrException()
val setResult = arr.ToHashSetOrException()
val dictResult = arr.ToDictionaryOrException((x: int) => x, (x: int) => x.ToString())

val total = arrResult.Match(ok => ok.Length, error => 0)
val listCount = listResult.Match(ok => ok.Count, error => 0)
val setCount = setResult.Match(ok => ok.Count, error => 0)
val dictCount = dictResult.Match(ok => ok.Count, error => 0)
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void MaterializationOrErrorExtensions_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val arr = [1, 2, 3]
val arrResult = arr.ToArrayOrError(ex => ex.Message)
val listResult = arr.ToListOrError(ex => ex.Message)
val setResult = arr.ToHashSetOrError(ex => ex.Message)
val dictResult = arr.ToDictionaryOrError((x: int) => x, (x: int) => x.ToString(), ex => ex.Message)

val total = arrResult.Match(ok => ok.Length, error => 0)
val listCount = listResult.Match(ok => ok.Count, error => 0)
val setCount = setResult.Match(ok => ok.Count, error => 0)
val dictCount = dictResult.Match(ok => ok.Count, error => 0)
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void ToDictionaryOrError_WithWrongErrorFactoryType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val arr = [1, 2, 3]
val _ = arr.ToDictionaryOrError((x: int) => x, (ex: int) => ex)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501")
                    .WithAnySpan()
                    .WithArguments("method", "ToDictionaryOrError", 2)
            ]);

        verifier.Verify();
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
                new DiagnosticResult("RAV0117")
                    .WithAnySpan()
                    .WithArguments("DoesNotExist")
            ]);

        verifier.Verify();
    }
}
