using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class StructuralTypeDisplayContractTests
{
    [Fact]
    [Trait("Target", "Shared")]
    public void StructuralUnionRetainsAlternativeOrder()
    {
        var union = Make<StructuralUnionTypeSymbol>("union", TypeKind.StructuralUnion);
        Set(union, "get_ConstituentTypes", ImmutableArray.Create(Type("A"), Type("B")));
        Assert.Equal("A | B", Display(union));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_IntersectionInUnionRetainsGrouping()
    {
        var intersection = Make<IntersectionTypeSymbol>("intersection", TypeKind.Intersection);
        Set(intersection, "get_ConstituentTypes", ImmutableArray.Create(Type("B"), Type("C")));
        var union = Make<StructuralUnionTypeSymbol>("union", TypeKind.StructuralUnion);
        Set(union, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(Type("A"), intersection));
        Assert.Equal("A | (B & C)", Display(union));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_StructuralUnionInIntersectionRetainsGrouping()
    {
        var union = Make<StructuralUnionTypeSymbol>("union", TypeKind.StructuralUnion);
        Set(union, "get_ConstituentTypes", ImmutableArray.Create(Type("A"), Type("B")));
        var intersection = Make<IntersectionTypeSymbol>("intersection", TypeKind.Intersection);
        Set(intersection, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(union, Type("C")));
        Assert.Equal("(A | B) & C", Display(intersection));
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void ReturnTypeProjectionPreservesItsOperand()
    {
        var projection = Make<FunctionReturnTypeSymbol>("ReturnType", TypeKind.FunctionReturn);
        Set(projection, "get_FunctionType", Type("F"));
        Assert.Equal("ReturnType<F>", Display(projection));
    }

    private static string Display(TypeSymbol type)
        => SymbolExtensions.ToDisplayStringForDiagnostics(type, SymbolDisplayFormat.MinimallyQualifiedFormat);

    private static TypeSymbol Type(string name) => Make<TypeSymbol>(name, TypeKind.Class);

    private static T Make<T>(string name, TypeKind kind) where T : class
    {
        var proxy = DispatchProxy.Create<T, SymbolProxy>();
        Set(proxy, "get_Name", name);
        Set(proxy, "get_TypeKind", kind);
        Set(proxy, "get_SpecialType", SpecialType.None);
        Set(proxy, "ToFullyQualifiedMetadataName", name);
        return proxy;
    }

    private static void Set(object target, string name, object? value)
        => ((SymbolProxy)target).Values[name] = value;

    public class SymbolProxy : DispatchProxy
    {
        public Dictionary<string, object?> Values { get; } = new();
        protected override object? Invoke(MethodInfo? method, object?[]? args)
            => Values.TryGetValue(method!.Name, out var value) ? value : throw new InvalidOperationException(method.Name);
    }
}
