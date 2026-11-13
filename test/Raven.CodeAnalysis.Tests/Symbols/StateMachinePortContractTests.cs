using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class StateMachinePortContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AsyncConstructionRetainsPolicyOwnerAndHoistedFields(bool heap)
    {
        var services = Services(heap);
        var method = Method();
        var state = new SynthesizedAsyncStateMachineTypeSymbol(services, method, "State");
        Assert.Equal(heap ? TypeKind.Class : TypeKind.Struct, state.TypeKind);
        Assert.Same(services, state.Services);
        Assert.Same(method, state.AsyncMethod);
        Assert.Same(state, state.Constructor.ContainingType);
        Assert.Same(state, state.GetConstructedStateMachine(method));
        var type = services.GetSpecialType(SpecialType.System_Int32);
        var field = state.AddHoistedLocal("temp", type, requiresDispose: true);
        Assert.Same(type, field.Type);
        Assert.Same(state, field.ContainingType);
        Assert.Same(field, Assert.Single(state.HoistedLocals));
        Assert.Same(field, Assert.Single(state.HoistedLocalsToDispose));
        Assert.Equal(state.GetBuilderMembers(method), state.GetBuilderMembers(method));
        Assert.Throws<ArgumentException>(() => state.GetConstructedStateMachine(Method()));
    }

    [Fact]
    public void AsyncIteratorRetainsElementTypeAndMissingRuntimeFallbacks()
    {
        var services = Services(false);
        var method = Method();
        var element = services.GetSpecialType(SpecialType.System_Int32);
        var state = new SynthesizedIteratorTypeSymbol(services, method, "Iterator", IteratorMethodKind.AsyncEnumerator, element);
        Assert.Same(services, state.Services);
        Assert.Same(method, state.IteratorMethod);
        Assert.Same(element, state.ElementType);
        Assert.Same(element, state.CurrentField.Type);
        Assert.Same(state, state.GetConstructedStateMachine(method));
        Assert.NotNull(state.AsyncMoveNextMethod);
        Assert.NotNull(state.AsyncDisposeMethod);
        Assert.Null(state.DisposeMethod);
        Assert.Null(state.NonGenericCurrentProperty);
        Assert.Empty(state.Interfaces);
        var field = state.AddHoistedLocal("temp", element);
        Assert.Same(field, Assert.Single(state.HoistedLocals));
    }

    [Fact]
    public void AsyncGenericParametersMapInBothDirectionsAndCacheConstructedMembers()
    {
        var services = Services(false);
        var method = Method();
        var parameter = new SourceTypeParameterSymbol("T", method, null, null, [], [], 0,
            TypeParameterConstraintKind.None, [], VarianceKind.None);
        method.SetTypeParameters([parameter]);
        var state = new SynthesizedAsyncStateMachineTypeSymbol(services, method, "GenericState");
        var mapped = Assert.Single(state.TypeParameters);
        Assert.Same(state, mapped.ContainingSymbol);
        Assert.True(state.TryMapToStateMachineTypeParameter(parameter, out var stateParameter));
        Assert.Same(mapped, stateParameter);
        Assert.True(state.TryMapToAsyncMethodTypeParameter(mapped, out var sourceParameter));
        Assert.Same(parameter, sourceParameter);
        Assert.Same(mapped, state.SubstituteAsyncMethodTypeParameters(parameter));
        Assert.Same(parameter, state.SubstituteStateMachineTypeParameters(mapped));
        var returningParameter = new SourceMethodSymbol("Return", parameter, [], method, null, null, [], []);
        var mappedMethod = state.SubstituteAsyncMethodTypeParameters(returningParameter);
        Assert.NotSame(returningParameter, mappedMethod);
        Assert.Same(mapped, mappedMethod.ReturnType);
        Assert.Same(mappedMethod, state.SubstituteAsyncMethodTypeParameters(mappedMethod));
        Assert.Same(parameter, state.SubstituteStateMachineTypeParameters(mappedMethod).ReturnType);
        var first = state.GetConstructedMembers(method);
        var second = state.GetConstructedMembers(method);
        Assert.Same(first.StateMachineType, second.StateMachineType);
        Assert.Same(first.Constructor, second.Constructor);
        Assert.Same(first.BuilderField, second.BuilderField);
    }

    [Fact]
    public void StateMachineDataRecordsPreserveValueEqualityAndValidation()
    {
        var field = Make<FieldSymbol>(SymbolKind.Field, "builder");
        var builder = new SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers(field, null, null, null, null, null, null, null);
        var equalBuilder = new SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers(field, null, null, null, null, null, null, null);
        Assert.True(builder == equalBuilder);
        var otherField = Make<FieldSymbol>(SymbolKind.Field, "other");
        Assert.True(builder != new SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers(otherField, null, null, null, null, null, null, null));
        Assert.Equal(builder.GetHashCode(), equalBuilder.GetHashCode());
        var method = Method();
        var type = Named("Test", "State");
        var fields = ImmutableDictionary<ParameterSymbol, FieldSymbol>.Empty;
        var members = new SynthesizedAsyncStateMachineTypeSymbol.ConstructedMembers(type, method, method, field, field, null, fields, builder, builder);
        var equalMembers = new SynthesizedAsyncStateMachineTypeSymbol.ConstructedMembers(type, method, method, field, field, null, fields, equalBuilder, equalBuilder);
        Assert.True(members == equalMembers);
        Assert.True(members != new SynthesizedAsyncStateMachineTypeSymbol.ConstructedMembers(type, method, method, otherField, field, null, fields, builder, builder));
        Assert.Equal(members.GetHashCode(), equalMembers.GetHashCode());
        var source = MetadataSymbolFixture.Parameter("T", 0);
        var target = MetadataSymbolFixture.Parameter("TState", 0);
        var mapping = new SynthesizedAsyncStateMachineTypeSymbol.TypeParameterMapping(source, target);
        Assert.True(mapping == new SynthesizedAsyncStateMachineTypeSymbol.TypeParameterMapping(source, target));
        Assert.True(mapping != new SynthesizedAsyncStateMachineTypeSymbol.TypeParameterMapping(target, source));
        Assert.Throws<ArgumentNullException>(() => new SynthesizedAsyncStateMachineTypeSymbol.TypeParameterMapping(null!, target));
        Assert.Throws<ArgumentNullException>(() => new SynthesizedAsyncStateMachineTypeSymbol.TypeParameterMapping(source, null!));
    }

    private static SourceMethodSymbol Method()
        => new("Run", Named("Test", "Result"), [], Make<Symbol>(SymbolKind.Namespace, "Root"), null, null, [], [], isAsync: true);

    private static StateMachineServices Services(bool heap)
    {
        var services = Make<StateMachineServices>(SymbolKind.Namespace, "Services");
        var types = new Dictionary<SpecialType, NamedTypeSymbol>();
        NamedTypeSymbol Type(SpecialType special)
        {
            if (types.TryGetValue(special, out var existing))
                return existing;
            var type = Make<NamedTypeSymbol>(SymbolKind.Type, special.ToString(), TypeKind.Class);
            Set(type, "get_SpecialType", special);
            Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
            Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
            Set(type, "get_Arity", 0);
            Set(type, "get_ConstructedFrom", type);
            Set(type, "GetMembers", ImmutableArray<Symbol>.Empty);
            Set(type, "get_IsReferenceType", true);
            types.Add(special, type);
            return type;
        }
        Set(services, "get_UseHeapAsyncStateMachines", heap);
        Set(services, "get_CaptureAsyncExceptions", true);
        Set(services, "GetSpecialType", (Func<object?[]?, object?>)(args => Type((SpecialType)args![0]!)));
        Set(services, "GetTypeByMetadataName", null);
        var error = Make<TypeSymbol>(SymbolKind.Type, "Error", TypeKind.Error);
        Set(services, "get_ErrorTypeSymbol", error);
        return services;
    }
}
