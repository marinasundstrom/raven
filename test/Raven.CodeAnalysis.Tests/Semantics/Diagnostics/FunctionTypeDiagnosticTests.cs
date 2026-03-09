using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionTypeDiagnosticTests : CompilationTestBase
{
    [Fact]
    public void FunctionType_WithoutFuncKeyword_Succeeds()
    {
        var source = """
        val f: int -> int = x => x
        """;

        var (compilation, _) = CreateCompilation(source);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void LambdaArgument_TypeMismatch_UsesFunctionNotation()
    {
        var source = """
        func test(x: int -> int) {}

        func Main() {
            test(() => 1)
        }
        """;

        var (compilation, _) = CreateCompilation(source,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType, diagnostic.Descriptor);
        Assert.Equal("Cannot convert from '() -> int' to 'int -> int'", diagnostic.GetMessage());
    }
}
