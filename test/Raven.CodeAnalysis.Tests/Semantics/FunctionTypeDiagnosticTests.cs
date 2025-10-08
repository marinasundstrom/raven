using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionTypeDiagnosticTests : CompilationTestBase
{
    [Fact]
    public void LambdaArgument_TypeMismatch_UsesFunctionNotation()
    {
        var source = """
        func test(x: int -> int) {}

        func main() {
            test(() => 1)
        }
        """;

        var (compilation, _) = CreateCompilation(source);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType, diagnostic.Descriptor);
        Assert.Equal("Cannot convert from '() -> int' to 'int -> int'", diagnostic.GetMessage());
    }
}
