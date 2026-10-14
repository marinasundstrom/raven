using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class SourceStructuralUnionTests
{
    [Theory]
    [InlineData(TargetPlatform.NeoCLR, true, true)]
    [InlineData(TargetPlatform.NeoCLR, false, false)]
    [InlineData(TargetPlatform.DotNet, true, false)]
    public void UnionSyntax_UsesExplicitNativeContract(TargetPlatform target, bool enabled, bool structural)
    {
        var tree = SyntaxTree.ParseText("""
            interface A {}
            interface B {}
            class Box<T> {}
            func Echo(value: A | B) -> A | B { return value }
            func Wrap(value: Box<A | B>) -> Box<A | B> { return value }
            """);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetPlatform(target)
            .WithMetadataImportOptions(new MetadataImportOptions().WithStructuralCompositions(enabled));
        var compilation = Compilation.Create("source-unions", [tree], TestMetadataReferences.DefaultWithRavenCore, options);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<UnionTypeSyntax>())
        {
            var type = model.GetTypeInfo(syntax).Type;
            if (structural)
            {
                var union = Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(type);
                Assert.Equal(2, union.ConstituentTypes.Length);
                Assert.Empty(union.MetadataName);
                Assert.True(SymbolEqualityComparer.Default.Equals(union, model.GetTypeInfo(syntax).Type));
            }
            else
            {
                var carrier = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
                Assert.Equal("Union", carrier.Name);
            }
        }
    }
}
