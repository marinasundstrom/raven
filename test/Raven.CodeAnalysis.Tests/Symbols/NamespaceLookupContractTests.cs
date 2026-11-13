using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class NamespaceLookupContractTests
{
    [Fact]
    public void TypeSelectionDeduplicatesAndIgnoresNonNamedTypes()
    {
        var type = Type("A", 0);
        var parameter = Make<TypeParameterSymbol>(SymbolKind.TypeParameter, "T");
        Assert.Same(type, TypeLookupUtilities.SelectBestTypeByName([type, type, parameter]));
        Assert.Null(TypeLookupUtilities.SelectBestTypeByName([parameter]));
        Assert.Null(TypeLookupUtilities.SelectBestTypeByName([]));
    }

    [Fact]
    public void DifferentAritiesPreferZeroArityWithoutSelectingAnArbitraryGeneric()
    {
        var zero = Type("Zero", 0);
        var one = Type("One", 1);
        var two = Type("Two", 2);
        Assert.Same(zero, TypeLookupUtilities.SelectBestTypeByName([one, zero, two]));
        Assert.Null(TypeLookupUtilities.SelectBestTypeByName([one, two]));
        Assert.Same(two, TypeLookupUtilities.SelectBestNamedTypeByArity([zero, one, two], 2));
        Assert.Null(TypeLookupUtilities.SelectBestNamedTypeByArity([one], 2));
    }

    [Fact]
    public void ExactlyOneSourceDeclarationWinsAnOtherwiseAmbiguousLookup()
    {
        var metadata = Type("Metadata", 1);
        var source = Type("Source", 1, source: true);
        var otherSource = Type("OtherSource", 1, source: true);
        Assert.Same(source, TypeLookupUtilities.SelectBestTypeByName([metadata, source]));
        Assert.Same(source, TypeLookupUtilities.SelectBestNamedTypeByArity([metadata, source], 1));
        Assert.Null(TypeLookupUtilities.SelectBestTypeByName([metadata, source, otherSource]));
        Assert.Null(TypeLookupUtilities.SelectBestTypeByName([metadata, Type("OtherMetadata", 1)]));
    }

    [Fact]
    public void GlobalNamespaceProjectsDirectlyToTheRequestedModule()
    {
        var module = Make<ModuleSymbol>(SymbolKind.Module, "Target");
        var target = Namespace("", true);
        Set(module, "get_GlobalNamespace", target);
        Assert.Same(target, ModuleNamespaceResolver.Resolve(module, Namespace("", true)));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void QualifiedNamespaceMustBelongToTheRequestedModule(bool owned)
    {
        var module = Make<ModuleSymbol>(SymbolKind.Module, "Target");
        var root = Namespace("", true);
        var parent = Namespace("A", false);
        var child = Namespace("B", false);
        Set(module, "get_GlobalNamespace", root);
        Set(root, "LookupNamespace", (Func<object?[]?, object?>)(args => (string)args![0]! == "A" ? parent : null));
        Set(parent, "LookupNamespace", (Func<object?[]?, object?>)(args => (string)args![0]! == "B" ? child : null));
        Set(child, "get_ContainingModule", owned ? module : Make<ModuleSymbol>(SymbolKind.Module, "Other"));
        var requested = Namespace("A.B", false);
        Assert.Same(owned ? child : null, ModuleNamespaceResolver.Resolve(module, requested));
        Assert.Null(ModuleNamespaceResolver.Resolve(module, Namespace("A.Missing", false)));
        Assert.Null(ModuleNamespaceResolver.Resolve(module, Namespace("", false)));
        var unnamed = Namespace("Unnamed", false);
        Set(unnamed, "ToMetadataName", null);
        Assert.Null(ModuleNamespaceResolver.Resolve(module, unnamed));
    }

    private static NamedTypeSymbol Type(string name, int arity, bool source = false)
    {
        NamedTypeSymbol type = source ? Make<SourceNamedTypeIdentity>(SymbolKind.Type, name) : Make<NamedTypeSymbol>(SymbolKind.Type, name);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", arity);
        return type;
    }

    private static NamespaceSymbol Namespace(string name, bool global)
    {
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, name);
        Set(ns, "get_IsGlobalNamespace", global);
        Set(ns, "ToMetadataName", name);
        return ns;
    }
}
