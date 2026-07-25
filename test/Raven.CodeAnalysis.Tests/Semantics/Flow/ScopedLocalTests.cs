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

    [Theory]
    [InlineData("return local")]
    [InlineData("val alias = local\nreturn alias")]
    public void ScopedRefLikeLocal_CannotEscapeThroughReturn(string returnStatements)
    {
        var source = $$"""
            func Leak(value: System.Span<int>) -> System.Span<int> {
                scoped val local = value
                {{returnStatements}}
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV0355"));

        Assert.Contains("local", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void RefStructContainingScopedLocal_CannotEscapeThroughReturn()
    {
        const string source = """
            ref struct SpanHolder {
                field Value: System.Span<int>
            }

            func Leak(value: System.Span<int>) -> SpanHolder {
                scoped val local = value
                var holder = SpanHolder()
                holder.Value = local
                return holder
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV0355"));

        Assert.Contains("local", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void RefLikeCallResultDerivedFromScopedLocal_CannotEscape()
    {
        const string source = """
            func Identity(value: System.Span<int>) -> System.Span<int> {
                return value
            }

            func Leak(value: System.Span<int>) -> System.Span<int> {
                scoped val local = value
                return Identity(local)
            }
            """;

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor.Id == "RAV0355");
    }

    [Fact]
    public void ScopedCalleeParameter_DoesNotContributeToCallResultEscape()
    {
        const string source = """
            func SelectOther(
                scoped ignored: System.Span<int>,
                other: System.Span<int>
            ) -> System.Span<int> {
                return other
            }

            func Allowed(
                value: System.Span<int>,
                other: System.Span<int>
            ) -> System.Span<int> {
                scoped val local = value
                return SelectOther(local, other)
            }
            """;

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor.Id == "RAV0355");
    }
}
