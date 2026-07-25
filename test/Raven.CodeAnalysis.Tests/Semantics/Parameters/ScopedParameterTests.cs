using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ScopedParameterTests : CompilationTestBase
{
    [Fact]
    public void ScopedValueParameter_IsParsedAndClassified()
    {
        const string source = """
            func Consume(scoped value: System.Span<int>) {}
            """;

        var (compilation, tree) = CreateCompilation(source);
        var parameter = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParameterSyntax>()
            .Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(parameter.Parent!.Parent!));

        Assert.Equal(SyntaxKind.ScopedKeyword, parameter.ScopedKeyword.Kind);
        Assert.Equal(ScopedKind.ScopedValue, method.Parameters[0].ScopedKind);
    }

    [Fact]
    public void ScopedRefParameter_IsParsedAndClassified()
    {
        const string source = """
            func Consume(scoped ref value: int) {}
            """;

        var (compilation, tree) = CreateCompilation(source);
        var parameter = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParameterSyntax>()
            .Single();
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(SyntaxKind.ScopedKeyword, parameter.ScopedKeyword.Kind);
        Assert.Equal(RefKind.Ref, method.Parameters[0].RefKind);
        Assert.Equal(ScopedKind.ScopedRef, method.Parameters[0].ScopedKind);
    }

    [Theory]
    [InlineData("return value")]
    [InlineData("val alias = value\nreturn alias")]
    public void ScopedRefLikeParameter_CannotEscapeThroughReturn(string body)
    {
        var source = $$"""
            func Leak(scoped value: System.Span<int>) -> System.Span<int> {
                {{body}}
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV0353"));

        Assert.Contains("value", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void UnscopedRefLikeParameter_CanBeReturned()
    {
        const string source = """
            func Identity(value: System.Span<int>) -> System.Span<int> {
                return value
            }
            """;

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Descriptor.Id == "RAV0353");
    }
}
