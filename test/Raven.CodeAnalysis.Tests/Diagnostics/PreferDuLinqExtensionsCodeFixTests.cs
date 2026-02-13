using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferDuLinqExtensionsCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesFirstToFirstOrError()
    {
        var code = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.First()
}
""";

        var fixedCode = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.FirstOrError(() => "TODO: provide error")
}
""";

        var verifier = CreateCodeFixVerifier<PreferDuLinqExtensionsAnalyzer, PreferDuLinqExtensionsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferDuLinqExtensionsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_RewritesFirstOrDefaultToFirstOrNone()
    {
        var code = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.FirstOrDefault()
}
""";

        var fixedCode = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.FirstOrNone()
}
""";

        var verifier = CreateCodeFixVerifier<PreferDuLinqExtensionsAnalyzer, PreferDuLinqExtensionsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferDuLinqExtensionsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
