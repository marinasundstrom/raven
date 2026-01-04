using System.Linq;

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
    public void TypeParameterList_MissingComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("class Box<T U> {}");

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor);
    }

    [Fact]
    public void ArgumentList_AllowsNewlinesWithCommas()
    {
        var tree = SyntaxTree.ParseText(
            """
            Foo(
                1,
                2
            );
            """
        );

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void BracketedArgumentList_NewlineWithoutComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText(
            """
            func Main() {
                Foo[1
                    2];
            }
            """
        );

        var diagnostics = tree.GetDiagnostics().ToArray();

        Assert.InRange(diagnostics.Length, 1, 2);
        Assert.All(diagnostics, diagnostic => Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor));
    }

    [Fact]
    public void BracketedParameterList_NewlineWithoutComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText(
            """
            class C {
                public self[first: int
                            second: int]: int { get => 0 }
            }
            """
        );

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor);
    }

    [Fact]
    public void TypeParameterList_NewlineWithoutComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Box<T
                       U> {}
            """
        );

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor);
    }

    [Fact]
    public void TypeArgumentList_NewlineWithoutComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Holder {
                var value: Box<int
                                string>
            }
            """
        );

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CharacterExpected, diagnostic.Descriptor);
    }
}
