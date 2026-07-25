using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ScopedLocalTests : CompilationTestBase
{
    [Fact]
    public void ScopedRefLikeLocal_IsParsedAndClassified()
    {
        const string source = """
            func UseBuffer() {
                scoped val buffer: System.Span<int> = stackalloc int[4]
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclarationSyntax>()
            .Single();
        var declarator = Assert.Single(declaration.Declarators);
        var local = Assert.IsAssignableFrom<ILocalSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declarator));

        Assert.Equal(SyntaxKind.ScopedKeyword, declaration.ScopedKeyword.Kind);
        Assert.Equal(ScopedKind.ScopedValue, local.ScopedKind);
    }

    [Fact]
    public void ScopedReferenceLocal_IsParsedAndClassified()
    {
        const string source = """
            func UseReference() {
                val value = 1
                scoped val reference = &value
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclarationSyntax>()
            .Last();
        var declarator = Assert.Single(declaration.Declarators);
        var local = Assert.IsAssignableFrom<ILocalSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declarator));

        Assert.Equal(SyntaxKind.ScopedKeyword, declaration.ScopedKeyword.Kind);
        Assert.Equal(ScopedKind.ScopedRef, local.ScopedKind);
    }

    [Theory]
    [InlineData("scoped val value = 1")]
    [InlineData("scoped var value: string = \"text\"")]
    [InlineData("scoped const value = 1")]
    public void ScopedOrdinaryValueLocal_IsRejected(string declaration)
    {
        var source = $$"""
            func Invalid() {
                {{declaration}}
            }
            """;

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor.Id == "RAV0354");
    }
}
