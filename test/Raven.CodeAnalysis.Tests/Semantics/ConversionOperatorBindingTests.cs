using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConversionOperatorBindingTests : CompilationTestBase
{
    [Fact]
    public void ImplicitConversionOperator_AllowsOverloadResolutionAndExplicitCast()
    {
        const string source = """
        class Box {
            public static implicit operator(value: Box) -> string { return "" }
            public static explicit operator(value: Box) -> int { return 0 }
        }

        func takesString(value: string) -> () { }

        func test() -> () {
            let box: Box = default
            takesString(box)
            let number: int = (int)box
        }
        """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            d => d.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }
}
