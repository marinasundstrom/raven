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
}
