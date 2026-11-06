using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class SymbolTypeQueryContractTests
{
    [Fact]
    public void NullAndNonNullableContractsArePreserved()
    {
        Assert.False(SymbolExtensions.ContainsErrorType(null));
        Assert.Throws<ArgumentNullException>(() => SymbolExtensions.GetNullableUnderlyingType(null!));
        Assert.Null(SymbolExtensions.GetNullableUnderlyingType(Make<TypeSymbol>()));
    }

    [Fact]
    public void ErrorTypesAreFoundThroughTypeArgumentsAndArrays()
    {
        var error = Make<ErrorTypeSymbol>();
        var array = Make<ArrayTypeSymbol>(("get_ElementType", error));
        var named = Make<NamedTypeSymbol>(("get_TypeArguments", ImmutableArray.Create<TypeSymbol>(array)), ("get_TypeKind", TypeKind.Class));
        Assert.True(SymbolExtensions.ContainsErrorType(error));
        Assert.True(SymbolExtensions.ContainsErrorType(named));
    }

    [Fact]
    public void RecursiveConstraintsTerminateWithoutHidingLaterErrors()
    {
        var parameter = Make<TypeParameterSymbol>();
        Set(parameter, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(parameter));
        Assert.False(SymbolExtensions.ContainsErrorType(parameter));
        Set(parameter, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(parameter, Make<ErrorTypeSymbol>()));
        Assert.True(SymbolExtensions.ContainsErrorType(parameter));
    }

    [Fact]
    public void DelegateSignatureIncludesReturnAndParameterErrors()
    {
        var ordinary = Make<TypeSymbol>();
        var error = Make<ErrorTypeSymbol>();
        var parameter = Make<ParameterSymbol>(("get_Type", error));
        var invoke = Make<MethodSymbol>(("get_ReturnType", ordinary), ("get_Parameters", ImmutableArray.Create(parameter)));
        var callable = Make<NamedTypeSymbol>(("get_TypeKind", TypeKind.Delegate), ("get_TypeArguments", ImmutableArray<TypeSymbol>.Empty),
            ("GetMembers", ImmutableArray.Create<Symbol>(invoke)));
        Assert.Same(invoke, SymbolExtensions.GetDelegateInvokeMethod(callable));
        Assert.True(SymbolExtensions.ContainsErrorType(callable));
        Set(parameter, "get_Type", ordinary);
        Assert.False(SymbolExtensions.ContainsErrorType(callable));
        Set(invoke, "get_ReturnType", error);
        Assert.True(SymbolExtensions.ContainsErrorType(callable));
    }

    [Fact]
    public void InvokeLookupRequiresDelegateAndSelectsMethodMembers()
    {
        var ordinary = Make<NamedTypeSymbol>(("get_TypeKind", TypeKind.Class));
        Assert.Null(SymbolExtensions.GetDelegateInvokeMethod(ordinary));
        var invoke = Make<MethodSymbol>();
        var callable = Make<NamedTypeSymbol>(("get_TypeKind", TypeKind.Delegate),
            ("GetMembers", ImmutableArray.Create<Symbol>(Make<FieldSymbol>(), invoke)));
        Assert.Same(invoke, SymbolExtensions.GetDelegateInvokeMethod(callable));
    }

    [Theory]
    [InlineData("ILiteralTypeInfo")]
    [InlineData("IByRefTypeInfo")]
    [InlineData("ISynthesizedDelegateSignature")]
    public void RepresentationContractsUseOwnedSymbolTypes(string name)
    {
        var contract = typeof(SymbolExtensions).Assembly.GetType("Raven.CodeAnalysis.Symbols." + name, true)!;
        Assert.True(contract.IsInterface);
        Assert.False(contract.IsPublic);
        Assert.NotEmpty(contract.GetProperties());
        foreach (var property in contract.GetProperties())
        {
            Assert.Null(property.SetMethod);
            var type = property.PropertyType;
            foreach (var dependency in type.IsGenericType ? type.GetGenericArguments() : new[] { type })
                if (dependency.Namespace?.StartsWith("Raven.CodeAnalysis", StringComparison.Ordinal) == true)
                    Assert.Same(typeof(TypeSymbol).Assembly, dependency.Assembly);
        }
    }

    private static T Make<T>(params (string Name, object Value)[] values) where T : class
    {
        var value = DispatchProxy.Create<T, SymbolProxy>();
        foreach (var entry in values) Set(value, entry.Name, entry.Value);
        return value;
    }

    private static void Set(object value, string name, object result)
        => ((SymbolProxy)value).Values[name] = result;

    public class SymbolProxy : DispatchProxy
    {
        public Dictionary<string, object> Values { get; } = new();
        protected override object? Invoke(MethodInfo? method, object?[]? arguments)
            => Values.TryGetValue(method!.Name, out var value) ? value : throw new InvalidOperationException(method.Name);
    }
}
