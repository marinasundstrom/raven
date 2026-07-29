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

let arr = [1, 2, 3]
let first = arr.FirstOrNone(x => x > 1)
let last = arr.LastOrNone()
let only = arr.Where(x => x == 2).SingleOrError(() => "none", () => "many")
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void MaterializationOrExceptionExtensions_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

let arr = [1, 2, 3]
let arrResult = arr.ToArrayOrException()
let listResult = arr.ToListOrException()
let setResult = arr.ToHashSetOrException()
let dictResult = arr.ToDictionaryOrException((x: int) => x, (x: int) => x.ToString())

let total = arrResult.Match(ok => ok.Length, error => 0)
let listCount = listResult.Match(ok => ok.Count, error => 0)
let setCount = setResult.Match(ok => ok.Count, error => 0)
let dictCount = dictResult.Match(ok => ok.Count, error => 0)
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void MaterializationOrErrorExtensions_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

let arr = [1, 2, 3]
let arrResult = arr.ToArrayOrError(ex => ex.Message)
let listResult = arr.ToListOrError(ex => ex.Message)
let setResult = arr.ToHashSetOrError(ex => ex.Message)
let dictResult = arr.ToDictionaryOrError((x: int) => x, (x: int) => x.ToString(), ex => ex.Message)

let total = arrResult.Match(ok => ok.Length, error => 0)
let listCount = listResult.Match(ok => ok.Count, error => 0)
let setCount = setResult.Match(ok => ok.Count, error => 0)
let dictCount = dictResult.Match(ok => ok.Count, error => 0)
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void ToDictionaryOrError_WithWrongErrorFactoryType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

let arr = [1, 2, 3]
let _ = arr.ToDictionaryOrError((x: int) => x, (ex: int) => ex)
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

let arr = [1, 2, 3]
let _ = arr.DoesNotExist()
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0117")
                    .WithAnySpan()
                    .WithArguments("ImmutableList", "DoesNotExist")
            ]);

        verifier.Verify();
    }
}
