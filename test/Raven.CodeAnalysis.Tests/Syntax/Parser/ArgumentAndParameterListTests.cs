using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

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
    public void ArgumentList_IncrementalDeleteLastArgument_ReparsesEmptyArgumentList()
    {
        const string source = """
            func Main() {
                A(x)
            }
            """;
        var text = SourceText.From(source);
        var tree = SyntaxTree.ParseText(text);
        var xPosition = source.IndexOf("x", StringComparison.Ordinal);

        var updatedTree = tree.WithChangedText(text.WithChange(new TextChange(new TextSpan(xPosition, 1), string.Empty)));
        var invocation = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        Assert.Empty(invocation.ArgumentList.Arguments);
        Assert.Equal("A()", invocation.ToString());
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
