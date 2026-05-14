using System;

using Raven.CodeAnalysis.Semantics.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Binding;

public sealed class SignatureSkeletonBindingTests : CompilationTestBase
{
    [Fact]
    public void PrimaryConstructorMethodBody_DoesNotReportDuplicateSkeletonCandidate()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

union Option<T> {
    case Some(value: T)
    case None
}

val foo = Foo(42)
val result = await foo(true)

class Foo(value: int) {
    async func Test(flag: bool) -> Task<Option<int>> {
        await Task.Delay(20)

        if flag {
            return .Some(value)
        }

        return .None
    }

    func self(flag: bool) -> Task<Option<int>> {
        return Test(flag)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.ToString().Contains("ambiguous", StringComparison.OrdinalIgnoreCase));
    }
}
