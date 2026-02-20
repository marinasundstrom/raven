using System.Linq;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class GenericTypeArgumentRecoveryTests : CompilationTestBase
{
    [Fact]
    public void MalformedGenericTypeArgumentList_DoesNotThrowDuringBinding()
    {
        var source = """
            val test: Result<int, [string> = .Ok(42)
            """;

        var (compilation, _) = CreateCompilation(source);

        var exception = Record.Exception(() => compilation.GetDiagnostics().ToArray());

        Assert.Null(exception);
    }
}
