using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class RecordDeclarationParserTests : DiagnosticTestBase
{
    [Fact]
    public void RecordClassDeclaration_ParsesRecordDeclarationSyntax()
    {
        var source = "record class Person(name: string, age: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<RecordDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RecordDeclaration_WithoutClassKeyword_DefaultsToRecordClass()
    {
        var source = "record Person(name: string, age: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<RecordDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RecordStructDeclaration_WithPrimaryConstructor_ParsesParameterList()
    {
        var source = "record struct Point(x: int, y: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<RecordDeclarationSyntax>(Assert.Single(root.Members));

        Assert.True(declaration.ClassOrStructKeyword.IsKind(SyntaxKind.StructKeyword));
        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RecordClassDeclaration_WithMissingParameterTypeAfterColon_ReportsIdentifierExpected()
    {
        var source = """
            record class Foo(
                val A: ,
                val B: int
            )
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.IsType<RecordDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.IdentifierExpected.Id, diagnostic.Descriptor.Id);
    }

    [Fact]
    public void RecordPrimaryConstructorParameterSpans_DoNotOverflowAfterFunctionExpressionBlock()
    {
        var source = """
            import System.Console.*
            import System.Threading.Tasks.*

            async func Main() -> Task {
                val f = async func (a: int, b: int) {
                    await Task.FromResult(a + b)
                }

                val f1 = f(2, 3).ContinueWith(x => {
                    //x.Result
                    2
                })
            }

            record class Foo(
                val A: int,
                val B: int
            )
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var declaration = Assert.IsType<RecordDeclarationSyntax>(root.Members.Last());

        Assert.NotNull(declaration.ParameterList);
        var parameterList = declaration.ParameterList!;
        var closeParenStart = parameterList.CloseParenToken.Span.Start;

        for (var i = 0; i < parameterList.Parameters.Count; i++)
        {
            var parameter = parameterList.Parameters[i];
            var firstToken = parameter.GetFirstToken(includeZeroWidth: true);
            var lastToken = parameter.GetLastToken(includeZeroWidth: true);
            Assert.True(
                parameter.Span.End <= closeParenStart,
                $"Parameter #{i} span end {parameter.Span.End} must be <= close paren start {closeParenStart}. First={firstToken.Kind} Last={lastToken.Kind} LastSpan={lastToken.Span} LastFullSpan={lastToken.FullSpan}");
            Assert.True(
                parameter.FullSpan.End <= root.FullSpan.End,
                $"Parameter #{i} full span end {parameter.FullSpan.End} must be <= root full span end {root.FullSpan.End}.");
        }
    }
}
