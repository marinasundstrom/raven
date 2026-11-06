using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class SymbolMetadataIdentityContractTests
{
    [Fact]
    public void NullAndReferenceIdentityFollowTheOriginalContract()
    {
        var type = Named("A", "Example.Token");
        Assert.True(SymbolExtensions.MetadataIdentityEquals(null, null));
        Assert.True(SymbolExtensions.MetadataIdentityEquals(type, type));
        Assert.False(SymbolExtensions.MetadataIdentityEquals(type, null));
    }

    [Fact]
    public void NominalMetadataIdentityCanAgreeAcrossAssemblyOwners()
    {
        var left = Named("A", "Example.Token");
        var right = Named("B", "Example.Token");
        Assert.False(SymbolEqualityComparer.Default.Equals(left, right));
        Assert.True(SymbolExtensions.MetadataIdentityEquals(left, right));
        Assert.False(SymbolExtensions.MetadataIdentityEquals(left, Named("B", "Example.Other")));
    }

    [Theory]
    [InlineData(null, null, true)]
    [InlineData(3, 3, true)]
    [InlineData(3, 4, false)]
    [InlineData(null, 3, false)]
    public void ArrayMetadataIdentityPreservesFixedLengths(int? leftLength, int? rightLength, bool expected)
    {
        var left = Array(Named("A", "Example.Token"), leftLength);
        var right = Array(Named("B", "Example.Token"), rightLength);
        Assert.Equal(expected, SymbolExtensions.MetadataIdentityEquals(left, right));
    }

    [Fact]
    public void StructuralArgumentsDoNotFallBackToNominalMetadataNames()
    {
        var leftUnion = Make<StructuralUnionTypeSymbol>(SymbolKind.Type, "union", TypeKind.StructuralUnion);
        var rightUnion = Make<StructuralUnionTypeSymbol>(SymbolKind.Type, "union", TypeKind.StructuralUnion);
        Set(leftUnion, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(Named("A", "Example.Left"), Named("A", "Example.Shared")));
        Set(rightUnion, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(Named("A", "Example.Right"), Named("A", "Example.Shared")));
        var left = Constructed("A", leftUnion);
        var right = Constructed("B", rightUnion);
        Assert.False(SymbolExtensions.MetadataIdentityEquals(left, right));
    }

    private static NamedTypeSymbol Constructed(string assemblyName, TypeSymbol argument)
    {
        var definition = Named(assemblyName, "Example.Box`1");
        var type = Named(assemblyName, "Example.Box`1");
        Set(type, "get_IsGenericType", true);
        Set(type, "get_ConstructedFrom", definition);
        Set(type, "get_OriginalDefinition", definition);
        Set(type, "get_TypeArguments", ImmutableArray.Create(argument));
        return type;
    }

    private static NamedTypeSymbol Named(string assemblyName, string fullName)
    {
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, assemblyName);
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Example");
        Set(ns, "get_ContainingSymbol", assembly);
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, fullName.Split('.').Last(), TypeKind.Class);
        Set(type, "get_ContainingNamespace", ns);
        Set(type, "get_ContainingSymbol", ns);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "ToFullyQualifiedMetadataName", fullName);
        return type;
    }

    private static ArrayTypeSymbol Array(TypeSymbol element, int? length)
    {
        var type = Make<ArrayTypeSymbol>(SymbolKind.Type, "array", TypeKind.Array);
        Set(type, "get_ElementType", element);
        Set(type, "get_Rank", 1);
        Set(type, "get_FixedLength", length);
        Set(type, "get_IsFixedArray", length.HasValue);
        return type;
    }

    private static T Make<T>(SymbolKind kind, string name, TypeKind typeKind = default) where T : class
    {
        var value = DispatchProxy.Create<T, IdentityProxy>();
        Set(value, "get_Kind", kind);
        Set(value, "get_Name", name);
        Set(value, "get_MetadataName", name);
        Set(value, "get_TypeKind", typeKind);
        Set(value, "get_SpecialType", SpecialType.None);
        Set(value, "get_OriginalDefinition", value);
        Set(value, "get_IsAlias", false);
        Set(value, "get_IsGenericType", false);
        Set(value, "get_ContainingNamespace", null);
        Set(value, "get_ContainingSymbol", null);
        Set(value, "get_ContainingType", null);
        return value;
    }

    private static void Set(object value, string name, object? result) => ((IdentityProxy)value).Values[name] = result;

    public class IdentityProxy : DispatchProxy
    {
        public Dictionary<string, object?> Values { get; } = new();
        protected override object? Invoke(MethodInfo? method, object?[]? arguments)
            => Values.TryGetValue(method!.Name, out var value) ? value : throw new InvalidOperationException(method.Name);
    }
}
