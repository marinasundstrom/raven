using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ArgumentAndParameterListTests
{
    [Fact]
    public void ArgumentList_DuplicateNamedArguments_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("Foo(a: 1, a: 2);");

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.DuplicateNamedArgument, diagnostic.Descriptor);
    }

    [Fact]
    public void ParameterList_NewlineWithoutComma_ProducesDiagnostic()
    {
        var source = """
            class C(
                a: int
                b: int) {}
            """;

        var tree = SyntaxTree.ParseText(source);

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor);
    }

    [Fact]
    public void TypeArgumentList_MissingComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("class Box<T U> {}");

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor);
    }
}
