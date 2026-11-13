using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class NamespaceTraversalContractTests
{
    [Fact]
    public void NullAndDefaultMemberCollectionsAreEmpty()
    {
        Assert.Empty(NamespaceSymbolExtensions.GetAllMembersRecursive(null));
        Assert.Empty(NamespaceSymbolExtensions.GetAllTypesRecursive(null));
        Assert.Null(NamespaceSymbolExtensions.AsSourceNamespace(null));
        var ns = Namespace("Root");
        Set(ns, "GetMembers", default(ImmutableArray<Symbol>));
        Assert.Empty(NamespaceSymbolExtensions.GetAllMembersRecursive(ns));
        Assert.Empty(NamespaceSymbolExtensions.GetAllTypesRecursive(ns));
    }

    [Fact]
    public void MemberTraversalPreservesNamespaceDepthFirstOrderAndImmediateTypeMembers()
    {
        var field = Make<Symbol>(SymbolKind.Field, "field");
        var leaf = Type("Leaf", field);
        var outer = Type("Outer", leaf);
        var nested = Namespace("Nested", outer);
        var sibling = Type("Sibling");
        var root = Namespace("Root", null!, nested, sibling);
        Assert.Equal(new Symbol[] { nested, outer, leaf, sibling },
            NamespaceSymbolExtensions.GetAllMembersRecursive(root));
    }

    [Fact]
    public void TypeTraversalIncludesAllNestedTypesAndExcludesOtherMembers()
    {
        var field = Make<Symbol>(SymbolKind.Field, "field");
        var leaf = Type("Leaf", field);
        var inner = Type("Inner", leaf);
        var outer = Type("Outer", field, inner);
        var root = Namespace("Root", null!, field, Namespace("Nested", outer));
        Assert.Equal(new[] { outer, inner, leaf }, NamespaceSymbolExtensions.GetAllTypesRecursive(root));
    }

    [Fact]
    public void TypeDiscoveryDoesNotMaterializeOtherMembers()
    {
        var leaf = Type("Leaf");
        var owner = Make<IDiscoveryType>(SymbolKind.Type, "Owner", TypeKind.Class);
        Set(owner, "GetNestedTypesForDiscovery", new[] { leaf });
        // GetMembers intentionally has no implementation: invoking it fails the fixture.
        Assert.Equal(new NamedTypeSymbol[] { owner, leaf },
            NamespaceSymbolExtensions.GetAllTypesRecursive(Namespace("Root", owner)));
    }

    [Fact]
    public void TraversalDefersMemberLookupUntilEnumeration()
    {
        var ns = Namespace("Root");
        var sequence = NamespaceSymbolExtensions.GetAllTypesRecursive(ns);
        var type = Type("Later");
        Set(ns, "GetMembers", ImmutableArray.Create<Symbol>(type));
        Assert.Same(type, Assert.Single(sequence));
    }

    [Fact]
    public void SourceLookupAcceptsDirectAndMergedNamespaces()
    {
        var first = new SourceNamespaceSymbol("First", Make<Symbol>(SymbolKind.Assembly, "A"), null, [], []);
        var second = new SourceNamespaceSymbol("Second", Make<Symbol>(SymbolKind.Assembly, "B"), null, [], []);
        var metadata = Namespace("Metadata");
        Assert.Same(first, NamespaceSymbolExtensions.AsSourceNamespace(first));
        Assert.Same(first, NamespaceSymbolExtensions.AsSourceNamespace(new MergedNamespaceSymbol([metadata, first, second], null)));
        Assert.Null(NamespaceSymbolExtensions.AsSourceNamespace(metadata));
    }

    [Theory]
    [InlineData(null, false, "Item", "Item")]
    [InlineData(null, false, "", "")]
    [InlineData("Example", false, "Item", "Example.Item")]
    [InlineData("Example", false, "", "Example")]
    [InlineData("", false, "Item", "Item")]
    [InlineData("", true, "Item", "Item")]
    public void QualificationPreservesGlobalAndEmptyNameRules(string? metadataName, bool global, string name, string expected)
    {
        NamespaceSymbol? ns = null;
        if (metadataName is not null)
        {
            ns = Namespace("Parent");
            Set(ns, "ToMetadataName", metadataName);
            Set(ns, "get_IsGlobalNamespace", global);
        }
        Assert.Equal(expected, NamespaceSymbolExtensions.QualifyName(ns, name));
    }

    private static NamespaceSymbol Namespace(string name, params Symbol[] members)
    {
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, name);
        Set(ns, "GetMembers", members.ToImmutableArray());
        Set(ns, "get_IsGlobalNamespace", false);
        return ns;
    }

    private static NamedTypeSymbol Type(string name, params Symbol[] members)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Class);
        Set(type, "GetMembers", members.ToImmutableArray());
        return type;
    }

    internal interface IDiscoveryType : NamedTypeSymbol, INestedTypeDiscovery { }
}
