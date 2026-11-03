using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullableAliasSignatureTests : CompilationTestBase
{
    [Theory]
    [InlineData("System.String", "?")]
    [InlineData("System.String", "?[]")]
    [InlineData("System.String", "?[][]")]
    [InlineData("System.Int32", "?")]
    [InlineData("System.Int32", "?[]")]
    [InlineData("System.Int32", "?[][]")]
    [InlineData("System.Collections.Generic.List<string>", "?")]
    [InlineData("System.Collections.Generic.List<string>", "?[]")]
    [InlineData("System.Collections.Generic.List<string>", "?[][]")]
    [Trait("Target", "Shared")]
    public void NullableAlias_HasSameStructuralIdentityAndHash(string underlying, string suffix)
    {
        var (compilation, tree) = CreateCompilation($$"""
            alias A = {{underlying}}
            class Sample {
                func Accept(original: {{underlying}}{{suffix}}, aliased: A{{suffix}}, distinct: {{underlying}}) { }
            }
            """);
        var declaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var original = method.Parameters[0].Type;
        var aliased = method.Parameters[1].Type;
        var distinct = method.Parameters[2].Type;
        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(original, aliased));
        Assert.True(comparer.Equals(aliased, original));
        Assert.Equal(comparer.GetHashCode(original), comparer.GetHashCode(aliased));
        var set = new HashSet<Symbol>(comparer) { original };
        Assert.Contains(aliased, set);
        Assert.False(comparer.Equals(original, distinct));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "Shared")]
    public void NullableArrayAlias_OverrideMatchesBaseSignature(bool querySymbolFirst)
    {
        var (compilation, tree) = CreateCompilation("""
            namespace Shared {
                abstract class Green {
                    internal abstract func With(children: Green?[]) -> Green
                }
            }
            namespace Shared.Internal {
                alias G = Shared.Green
                class Node : Shared.Green {
                    internal override func With(children: G?[]) -> G => self
                }
            }
            """);
        var declaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Last();
        var model = compilation.GetSemanticModel(tree);
        if (querySymbolFirst)
            Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(declaration));
        Assert.True(method.IsOverride);
    }
}
