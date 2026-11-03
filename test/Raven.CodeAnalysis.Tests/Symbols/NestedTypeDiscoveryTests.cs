using System.Collections.Immutable;

using NSubstitute;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NestedTypeDiscoveryTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NonPeDiscoveryTraversesChildrenWithoutLoadingOrdinaryMembers(bool constructed)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var outer = new DiscoveryType(compilation, "Outer");
        var inner = new DiscoveryType(compilation, "Inner", outer);
        var leaf = new DiscoveryType(compilation, "Leaf", inner);
        outer.Children = [inner];
        inner.Children = [leaf];
        NamedTypeSymbol root = constructed ? new ConstructedNamedTypeSymbol(outer, []) : outer;

        var types = NamespaceWith(root).GetAllTypesRecursive().ToArray();

        Assert.Equal(new NamedTypeSymbol[] { root, inner, leaf }, types);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PeDiscoveryRetainsNestedDeclarationIdentityForConstructedOwners(bool constructed)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var definition = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        Assert.NotNull(definition);
        var expected = definition.GetTypeMembers("Enumerator").Single();
        var root = constructed
            ? (NamedTypeSymbol)definition.Construct(compilation.GetSpecialType(SpecialType.System_Int32))
            : definition;

        var types = NamespaceWith(root).GetAllTypesRecursive().ToArray();

        Assert.Same(root, types[0]);
        Assert.Same(expected, Assert.Single(types.Where(type => type.Name == "Enumerator")));
        Assert.Same(definition, expected.ContainingType);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SourceFallbackPreservesNestedContainingTypeSubstitution(bool constructed)
    {
        var compilation = Compilation.Create("test", [SyntaxTree.ParseText("""
            class Outer<T> {
                class Inner {
                    class Leaf {}
                }
            }
            """)], TestMetadataReferences.Default);
        var definition = compilation.GetTypeByMetadataName("Outer`1")!;
        Assert.NotNull(definition);
        var root = constructed
            ? (NamedTypeSymbol)definition.Construct(compilation.GetSpecialType(SpecialType.System_Int32))
            : definition;

        var types = NamespaceWith(root).GetAllTypesRecursive().ToArray();

        Assert.Equal(new[] { "Outer", "Inner", "Leaf" }, types.Select(type => type.Name));
        Assert.True(SymbolEqualityComparer.Default.Equals(root, types[1].ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(types[1], types[2].ContainingType));
    }

    private static NamespaceSymbol NamespaceWith(NamedTypeSymbol root)
    {
        var symbol = Substitute.For<NamespaceSymbol>();
        symbol.GetMembers().Returns(ImmutableArray.Create<Symbol>(root));
        return symbol;
    }

    private sealed class DiscoveryType(Compilation compilation, string name, NamedTypeSymbol? owner = null)
        : SourceNamedTypeSymbol(name, compilation.GetSpecialType(SpecialType.System_Object), TypeKind.Class,
            (Symbol?)owner ?? compilation.Assembly, owner, compilation.SourceGlobalNamespace, [], [], addAsMember: false),
            INestedTypeDiscovery, NamespaceOrTypeSymbol
    {
        internal ImmutableArray<NamedTypeSymbol> Children { get; set; } = [];
        public IEnumerable<NamedTypeSymbol> GetNestedTypesForDiscovery() => Children;
        ImmutableArray<Symbol> NamespaceOrTypeSymbol.GetMembers()
            => throw new InvalidOperationException("Type discovery must not load ordinary members.");
    }
}
