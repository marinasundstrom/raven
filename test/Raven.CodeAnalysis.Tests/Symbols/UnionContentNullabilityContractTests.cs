using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class UnionContentNullabilityContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PlainAndNullTypesKeepIdentity(bool isNull)
    {
        var type = Type(isNull ? "null" : "A", isNull ? TypeKind.Null : TypeKind.Class);
        Assert.Same(type, UnionContentNullability.GetNonNullContentType(type, out var mayBeNull));
        Assert.Equal(isNull, mayBeNull);
        Assert.Equal(isNull, UnionContentNullability.IsNullableContentType(type));
    }

    [Fact]
    public void NullableProviderExposesUnderlyingTypeWithoutConcreteStorage()
    {
        var underlying = Type("A");
        var nullable = Nullable(underlying);
        Assert.Same(underlying, UnionContentNullability.GetNonNullContentType(nullable, out var mayBeNull));
        Assert.True(mayBeNull);
        Assert.True(UnionContentNullability.IsNullableContentType(nullable));
    }

    [Fact]
    public void AliasesAreUnwrappedOutsideAndInsideNullableProviders()
    {
        var underlying = Type("A");
        var nullable = Nullable(Alias(underlying));
        Assert.Same(underlying, UnionContentNullability.GetNonNullContentType(Alias(nullable), out var mayBeNull));
        Assert.True(mayBeNull);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NamedNullableRequiresOneTypeArgument(bool constructed)
    {
        var underlying = Type("A");
        var named = Make<NamedTypeSymbol>("Nullable", TypeKind.Struct);
        Set(named, "get_SpecialType", SpecialType.System_Nullable_T);
        Set(named, "get_TypeArguments", constructed ? ImmutableArray.Create<TypeSymbol>(underlying) : ImmutableArray<TypeSymbol>.Empty);
        Assert.Same(constructed ? underlying : named, UnionContentNullability.GetNonNullContentType(named, out var mayBeNull));
        Assert.Equal(constructed, mayBeNull);
    }

    [Fact]
    public void PatternDomainDeduplicatesUnwrappedMembersAndNull()
    {
        var first = Type("A");
        var second = Type("B");
        var nullType = Type("null", TypeKind.Null);
        var union = Make<UnionSymbol>("Union");
        Set(union, "get_MemberTypes", ImmutableArray.Create<TypeSymbol>(first, Nullable(first), nullType, Alias(first), second));
        Set(union, "get_ContentMayBeNull", false);
        Assert.Equal(new[] { first, second, nullType }, UnionContentNullability.GetPatternDomainTypes(union, nullType));
    }

    [Fact]
    public void ExplicitContentNullabilityAddsNullToPatternDomain()
    {
        var first = Type("A");
        var nullType = Type("null", TypeKind.Null);
        var union = Make<UnionSymbol>("Union");
        Set(union, "get_MemberTypes", ImmutableArray.Create(first));
        Set(union, "get_ContentMayBeNull", true);
        Assert.Equal(new[] { first, nullType }, UnionContentNullability.GetPatternDomainTypes(union, nullType));
    }

    private static TypeSymbol Type(string name, TypeKind kind = TypeKind.Class) => Make<TypeSymbol>(name, kind);

    private static TypeSymbol Nullable(TypeSymbol underlying)
    {
        var nullable = Make<NullableProvider>("NullableView");
        Set(nullable, "get_UnderlyingType", underlying);
        return nullable;
    }

    private static TypeSymbol Alias(TypeSymbol underlying)
    {
        var alias = Make<AliasProvider>("Alias");
        Set(alias, "get_IsAlias", true);
        Set(alias, "get_UnderlyingSymbol", underlying);
        return alias;
    }

    private static T Make<T>(string name, TypeKind kind = TypeKind.Class) where T : class
    {
        var value = DispatchProxy.Create<T, ProviderProxy>();
        Set(value, "get_Kind", SymbolKind.Type);
        Set(value, "get_TypeKind", kind);
        Set(value, "get_Name", name);
        Set(value, "get_MetadataName", name);
        Set(value, "get_SpecialType", SpecialType.None);
        Set(value, "get_IsNullable", false);
        Set(value, "get_IsAlias", false);
        Set(value, "get_OriginalDefinition", value);
        Set(value, "get_ContainingSymbol", null);
        Set(value, "get_ContainingNamespace", null);
        Set(value, "get_ContainingType", null);
        return value;
    }

    private static void Set(object value, string name, object? result) => ((ProviderProxy)value).Values[name] = result;

    internal interface NullableProvider : TypeSymbol, INullableTypeIdentity { }
    internal interface AliasProvider : TypeSymbol, AliasSymbol { }

    public class ProviderProxy : DispatchProxy
    {
        public Dictionary<string, object?> Values { get; } = new();
        protected override object? Invoke(MethodInfo? method, object?[]? arguments)
            => Values.TryGetValue(method!.Name, out var value) ? value : throw new InvalidOperationException(method.Name);
    }
}
