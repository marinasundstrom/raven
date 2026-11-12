using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class NullableFoundationContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NullableDecorationIsIdempotentAndReversible(bool valueType)
    {
        var type = Type(valueType);
        var nullable = TypeSymbolNullabilityExtensions.GetNullableType(type);
        Assert.True(nullable.IsNullable);
        Assert.Same(nullable, TypeSymbolNullabilityExtensions.GetNullableType(nullable));
        Assert.Same(type, TypeSymbolNullabilityExtensions.GetNonNullableType(nullable));
        Assert.Same(type, TypeSymbolNullabilityExtensions.GetNonNullableType(type));
        Assert.True(TypeSymbolNullabilityExtensions.TryGetNullableUnderlyingType(nullable, out var underlying));
        Assert.Same(type, underlying);
        Assert.False(TypeSymbolNullabilityExtensions.TryGetNullableUnderlyingType(type, out var absent));
        Assert.Null(absent);
        Assert.Equal(valueType ? NullableAbiProjection.NullableValueType : NullableAbiProjection.AnnotatedUnderlyingType,
            TypeSymbolNullabilityExtensions.GetNullableAbiProjection(nullable));
        Assert.Equal(NullableAbiProjection.None, TypeSymbolNullabilityExtensions.GetNullableAbiProjection(type));
    }

    [Fact]
    public void WrapperPreservesOwnershipWithoutBindingDeclarations()
    {
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, "Assembly");
        var module = Make<ModuleSymbol>(SymbolKind.Module, "Module");
        var owner = Make<NamespaceSymbol>(SymbolKind.Namespace, "Owner");
        Set(owner, "get_ContainingAssembly", assembly);
        Set(owner, "get_ContainingModule", module);
        var underlying = Type(false);
        var nullable = new NullableTypeSymbol(underlying, owner, null, owner, []);
        Assert.Same(owner, nullable.ContainingNamespace);
        Assert.Same(owner, nullable.ContainingSymbol);
        Assert.Same(assembly, nullable.ContainingAssembly);
        Assert.Same(module, nullable.ContainingModule);
        Assert.Empty(nullable.DeclaringSyntaxReferences);
        Assert.Empty(nullable.GetAttributes());
        Assert.Null(nullable.GetDocumentationComment());
        Assert.Equal(Accessibility.NotApplicable, nullable.DeclaredAccessibility);
    }

    [Fact]
    public void SubstitutionKeepsExplicitAbiProjectionAndOwners()
    {
        var originalType = Type(false);
        var owner = Make<NamespaceSymbol>(SymbolKind.Namespace, "Owner");
        var original = new NullableTypeSymbol(originalType, owner, null, owner, [], NullableAbiProjection.AnnotatedUnderlyingType);
        var replacement = Type(true);
        var substituted = TypeSymbolNullabilityExtensions.ApplySubstitutedNullability(replacement, original);
        var nullable = Assert.IsType<NullableTypeSymbol>(substituted);
        Assert.Same(replacement, nullable.UnderlyingType);
        Assert.Same(owner, nullable.ContainingNamespace);
        Assert.Equal(NullableAbiProjection.AnnotatedUnderlyingType, nullable.AbiProjection);
        Assert.Same(nullable, TypeSymbolNullabilityExtensions.ApplySubstitutedNullability(nullable, original));
        var method = Method(nullable);
        Assert.Same(replacement, NullableMetadataFacts.GetReturnType(method));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MaybeNullReturnAnnotationOnlyDecoratesReferenceTypes(bool valueType)
    {
        var type = Type(valueType);
        var method = Method(type);
        Set(method, "GetReturnTypeAttributes", ImmutableArray.Create(Attribute("MaybeNullAttribute")));
        var result = NullableMetadataFacts.GetReturnType(method);
        if (valueType)
            Assert.Same(type, result);
        else
            Assert.Same(type, Assert.IsType<NullableTypeSymbol>(result).UnderlyingType);
    }

    [Theory]
    [InlineData("AllowNullAttribute", true)]
    [InlineData("DisallowNullAttribute", false)]
    [InlineData("UnrelatedAttribute", false)]
    public void InputAttributesControlNullableDecoration(string name, bool expectedNullable)
    {
        var type = Type(false);
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Set(parameter, "get_Type", type);
        Set(parameter, "GetAttributes", ImmutableArray.Create(Attribute(name)));
        Assert.Equal(expectedNullable, NullableMetadataFacts.ParameterAllowsNullInput(parameter));
        Assert.Equal(expectedNullable, NullableMetadataFacts.GetInputType(parameter, type).IsNullable);
    }

    [Fact]
    public void DisallowNullOverridesAllowNullOnSetterParameter()
    {
        var type = Type(false);
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Set(parameter, "GetAttributes", ImmutableArray.Create(Attribute("DisallowNullAttribute")));
        var setter = Method(type);
        Set(setter, "get_Parameters", ImmutableArray.Create(parameter));
        var property = Make<PropertySymbol>(SymbolKind.Property, "Value");
        Set(property, "get_SetMethod", setter);
        Set(property, "GetAttributes", ImmutableArray.Create(Attribute("AllowNullAttribute")));
        Assert.False(NullableMetadataFacts.SymbolAllowsNullInput(property, type));
        Assert.Same(type, NullableMetadataFacts.GetInputType(property, type));
    }

    [Fact]
    public void SameNamedAttributeInAnotherNamespaceIsIgnored()
    {
        var type = Type(false);
        var method = Method(type);
        Set(method, "GetReturnTypeAttributes", ImmutableArray.Create(Attribute("MaybeNullAttribute", "Other")));
        Assert.Same(type, NullableMetadataFacts.GetReturnType(method));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TypeParameterValueConstraintControlsAbi(bool valueConstrained)
    {
        var parameter = Make<TypeParameterSymbol>(SymbolKind.Type, "T", TypeKind.TypeParameter);
        Set(parameter, "get_ConstraintKind", valueConstrained ? TypeParameterConstraintKind.ValueType : TypeParameterConstraintKind.None);
        Assert.Equal(valueConstrained ? NullableAbiProjection.NullableValueType : NullableAbiProjection.AnnotatedUnderlyingType,
            TypeSymbolNullabilityExtensions.InferNullableAbiProjection(parameter));
    }

    [Theory]
    [InlineData(TypeKind.Error)]
    [InlineData(TypeKind.Null)]
    [InlineData(TypeKind.Pointer)]
    public void DefaultValueKeepsSpecialRepresentations(TypeKind kind)
    {
        var type = Make<TypeSymbol>(SymbolKind.Type, "special", kind);
        Set(type, "get_IsNullable", false);
        Assert.Same(type, TypeSymbolNullabilityExtensions.GetDefaultValueType(type));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DefaultValueOnlyDecoratesReferenceTypes(bool valueType)
    {
        var type = Type(valueType);
        var result = TypeSymbolNullabilityExtensions.GetDefaultValueType(type);
        if (valueType)
            Assert.Same(type, result);
        else
        {
            Assert.True(result.IsNullable);
            Assert.Same(type, TypeSymbolNullabilityExtensions.GetNonNullableType(result));
        }
    }

    [Fact]
    public void NullableApisRejectNullArguments()
    {
        Assert.Throws<ArgumentNullException>(() => TypeSymbolNullabilityExtensions.GetNullableType(null!));
        Assert.Throws<ArgumentNullException>(() => TypeSymbolNullabilityExtensions.GetNonNullableType(null!));
        Assert.Throws<ArgumentNullException>(() => TypeSymbolNullabilityExtensions.GetNullableAbiProjection(null!));
        Assert.Throws<ArgumentNullException>(() => TypeSymbolNullabilityExtensions.TryGetNullableUnderlyingType(null!, out _));
    }

    [Fact]
    public void AbsoluteBaseTypeWalksToRootAndKeepsDetachedNamedType()
    {
        var root = Type(false);
        var parent = Type(false);
        var child = Type(false);
        Set(parent, "get_BaseType", root);
        Set(child, "get_BaseType", parent);
        var nullable = new NullableTypeSymbol(child, null, null, null, []);
        Assert.Same(root, nullable.BaseType);
        Assert.Same(root, new NullableTypeSymbol(root, null, null, null, []).BaseType);
    }

    private static NamedTypeSymbol Type(bool valueType)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, "Value", valueType ? TypeKind.Struct : TypeKind.Class);
        Set(type, "get_IsValueType", valueType);
        Set(type, "get_IsReferenceType", !valueType);
        Set(type, "get_IsNullable", false);
        Set(type, "get_BaseType", null);
        return type;
    }

    private static MethodSymbol Method(TypeSymbol returnType)
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, "Method");
        Set(method, "get_ReturnType", returnType);
        Set(method, "GetReturnTypeAttributes", ImmutableArray<AttributeData>.Empty);
        return method;
    }

    private static AttributeData Attribute(string name, string ns = "System.Diagnostics.CodeAnalysis")
    {
        var scope = Make<NamespaceSymbol>(SymbolKind.Namespace, ns);
        Set(scope, "ToMetadataName", ns);
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Class);
        Set(type, "get_ContainingNamespace", scope);
        return new AttributeData(type, Make<MethodSymbol>(SymbolKind.Method, ".ctor"), [], [], null);
    }
}
