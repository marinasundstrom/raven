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
    public void ParameterList_NewlineWithoutComma_UsesImplicitSeparator()
    {
        var source = """
            class C(
                a: int
                b: int) {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.Empty(tree.GetDiagnostics());
        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(SyntaxKind.None, declaration.ParameterList!.Parameters.GetSeparator(0).Kind);
    }

    [Fact]
    public void TypeParameterList_SameLineWithoutComma_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("class Box<T U> {}");
        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.Contains(tree.GetDiagnostics(), diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CharacterExpected);
        Assert.NotNull(declaration.TypeParameterList);
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
    public void BracketedParameterList_NewlineWithoutComma_UsesImplicitSeparator()
    {
        var tree = SyntaxTree.ParseText(
            """
            class C {
                var self[first: int
                            second: int]: int { get => 0 }
            }
            """
        );

        var indexer = Assert.IsType<IndexerDeclarationSyntax>(tree.GetRoot().DescendantNodes().OfType<IndexerDeclarationSyntax>().Single());

        Assert.Empty(tree.GetDiagnostics());
        Assert.Equal(SyntaxKind.None, indexer.ParameterList.Parameters.GetSeparator(0).Kind);
    }

    [Fact]
    public void TypeParameterList_NewlineWithoutComma_UsesImplicitSeparator()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Box<T
                       U> {}
            """
        );
        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.Empty(tree.GetDiagnostics());
        Assert.NotNull(declaration.TypeParameterList);
        Assert.Equal(SyntaxKind.None, declaration.TypeParameterList!.Parameters.GetSeparator(0).Kind);
    }

    [Fact]
    public void TypeArgumentList_NewlineWithoutComma_UsesImplicitSeparator()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Holder {
                var value: Box<int
                                string>
            }
            """
        );
        var genericName = Assert.IsType<GenericNameSyntax>(tree.GetRoot().DescendantNodes().OfType<GenericNameSyntax>().Single());

        Assert.Empty(tree.GetDiagnostics());
        Assert.Equal(SyntaxKind.None, genericName.TypeArgumentList.Arguments.GetSeparator(0).Kind);
    }

    [Fact]
    public void TypeArgumentList_NestedGenericClosersWithoutWhitespace_Parses()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Holder {
                var value: Box<Box<int>>
            }
            """
        );

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TypeArgumentList_AllowsFunctionTypeArgumentEndingWithNestedGenericClosers()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Holder {
                var value: Box<() -> Result<string, MyError>>
            }
            """
        );

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ParameterList_LeadingComma_ProducesDiagnosticsAndCompletesParse()
    {
        var tree = SyntaxTree.ParseText("func F(, value: int) {}");
        var root = tree.GetRoot();

        var function = Assert.IsType<GlobalStatementSyntax>(Assert.Single(root.Members)).Statement;
        Assert.IsType<FunctionStatementSyntax>(function);

        var diagnostics = tree.GetDiagnostics().ToArray();
        Assert.NotEmpty(diagnostics);
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.IdentifierExpected);
    }
}
