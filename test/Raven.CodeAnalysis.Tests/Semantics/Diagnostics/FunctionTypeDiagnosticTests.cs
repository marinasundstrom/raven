using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionTypeDiagnosticTests : CompilationTestBase
{
    [Fact]
    public void FunctionType_WithoutFuncKeyword_ProducesDiagnostic()
    {
        var source = """
        val f: int -> int = x => x
        """;

        var (compilation, _) = CreateCompilation(source);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.FunctionTypeSignatureMustStartWithFuncKeyword, diagnostic.Descriptor);
    }

    [Fact]
    public void LambdaArgument_TypeMismatch_UsesFunctionNotation()
    {
        var source = """
        func test(x: func int -> int) {}

        func Main() {
            test(() => 1)
        }
        """;

        var (compilation, _) = CreateCompilation(source,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType, diagnostic.Descriptor);
        Assert.Equal("Cannot convert from 'func () -> int' to 'func int -> int'", diagnostic.GetMessage());
    }
}
