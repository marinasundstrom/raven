using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class TypeNormalizationContractTests
{
    [Fact]
    public void EmptyInputUsesErrorSymbolOrThrowsWithParameterName()
    {
        var error = Named("Error", kind: TypeKind.Error);
        Assert.Same(error, TypeSymbolNormalization.GetBestCommonType([], errorTypeSymbol: error));
        Assert.Equal("types", Assert.Throws<ArgumentException>(() => TypeSymbolNormalization.GetBestCommonType([])).ParamName);
    }

    [Fact]
    public void LiteralAndAddressNormalizationPreserveUnderlyingType()
    {
        var type = Named("Value");
        var literal = new LiteralTypeSymbol(type, 42, Make<AssemblySymbol>(SymbolKind.Assembly, "Owner"));
        Assert.Same(type, TypeSymbolNormalization.NormalizeForInference(literal));
        var address = Make<AddressTypeSymbol>(SymbolKind.Type, "Address");
        Set(address, "get_ReferencedType", literal);
        var reference = Assert.IsType<RefTypeSymbol>(TypeSymbolNormalization.NormalizeForInference(address));
        Assert.Same(type, reference.ElementType);
        Assert.Same(type, TypeSymbolNormalization.GetBestCommonType([literal, type]));
        Assert.Same(type, TypeSymbolNormalization.GetBestCommonType([literal, literal]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ClosestBaseWinsOverInterfaces(bool reverse)
    {
        var root = ObjectType();
        var parent = Named("Parent", root);
        var contract = Named("Contract", kind: TypeKind.Interface);
        var left = Named("Left", parent, interfaces: [contract]);
        var right = Named("Right", parent, interfaces: [contract]);
        Assert.Same(parent, Infer(left, right, reverse));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MostSpecificCommonInterfaceWinsOverObject(bool reverse)
    {
        var root = ObjectType();
        var parent = Named("ParentContract", kind: TypeKind.Interface);
        var child = Named("ChildContract", kind: TypeKind.Interface, interfaces: [parent]);
        var left = Named("Left", root, interfaces: [parent, child]);
        var right = Named("Right", root, interfaces: [parent, child]);
        Assert.Same(child, Infer(left, right, reverse));
    }

    [Fact]
    public void UnrelatedCommonInterfacesFallBackToObject()
    {
        var root = ObjectType();
        var first = Named("First", kind: TypeKind.Interface);
        var second = Named("Second", kind: TypeKind.Interface);
        Assert.Same(root, Infer(Named("Left", root, interfaces: [first, second]), Named("Right", root, interfaces: [first, second])));
    }

    [Fact]
    public void NonNamedOperandsParticipateInNominalInference()
    {
        var root = ObjectType();
        var array = Make<TypeSymbol>(SymbolKind.Type, "Array", TypeKind.Array);
        Configure(array, root);
        Assert.Same(root, Infer(array, Named("Text", root)));
        Assert.Same(root, Infer(array, Named("Text", root), true));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NullCollapsesWithValueAndRedundantNullableCandidates(bool valueType)
    {
        var type = Named("Value", kind: valueType ? TypeKind.Struct : TypeKind.Class);
        var nullType = Named("null", kind: TypeKind.Null);
        var nullable = Assert.IsType<NullableTypeSymbol>(Infer(nullType, type));
        Assert.Same(type, nullable.UnderlyingType);
        Assert.Same(nullable, TypeSymbolNormalization.GetBestCommonType([type, nullable, nullType]));
    }

    [Fact]
    public void NullableReferenceParticipatesInNominalInference()
    {
        var root = ObjectType();
        var nullable = new NullableTypeSymbol(root, null, null, null, []);
        var result = Assert.IsType<NullableTypeSymbol>(Infer(Named("Text", root), nullable));
        Assert.Same(root, result.UnderlyingType);
    }

    [Fact]
    public void NotNullConstraintReportsDiagnosticAndReturnsProvidedError()
    {
        var parameter = Parameter("T", 0);
        Configure(parameter, null);
        Set(parameter, "get_ConstraintKind", TypeParameterConstraintKind.NotNull);
        var diagnostics = new DiagnosticBag();
        var error = Named("Error", kind: TypeKind.Error);
        Assert.Same(error, TypeSymbolNormalization.GetBestCommonType([parameter, Named("null", kind: TypeKind.Null)],
            diagnostics, Location.None, error));
        Assert.Single(diagnostics.AsEnumerable());
    }

    [Fact]
    public void NoNominalAncestorUsesAssemblyObjectOrExplicitFallback()
    {
        var first = Named("First");
        var second = Named("Second");
        var error = Named("Error", kind: TypeKind.Error);
        Assert.Same(error, TypeSymbolNormalization.GetBestCommonType([first, second], errorTypeSymbol: error));
        Assert.Same(first, Infer(first, second));
        var root = ObjectType();
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, "Owner");
        Set(assembly, "GetTypeByMetadataName", root);
        Set(second, "get_ContainingAssembly", assembly);
        Assert.Same(root, Infer(first, second));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PartiallyConstructedUnionArmsMergeConcreteArguments(bool reverse)
    {
        var t = Parameter("T", 0);
        var e = Parameter("E", 1);
        var definition = Union("Choice", [t, e]);
        var value = Named("Value");
        var error = Named("Failure");
        var left = Union("Choice", [t, e], [value, e], definition);
        var right = Union("Choice", [t, e], [t, error], definition);
        var expected = Union("Choice", [t, e], [value, error], definition);
        Set(definition, "Construct", (Func<object?[]?, object?>)(arguments =>
        {
            Assert.Equal(new TypeSymbol[] { value, error }, Assert.IsType<TypeSymbol[]>(arguments![0]));
            return expected;
        }));
        Assert.Same(expected, Infer(left, right, reverse));
    }

    [Fact]
    public void CaseArgumentsUseExplicitUnionParameterMapping()
    {
        var t = Parameter("T", 0);
        var e = Parameter("E", 1);
        var definition = Union("Choice", [t, e]);
        var caseParameter = Parameter("T", 0);
        var variant = Make<MappedCase>(SymbolKind.Type, "Failure");
        Configure(variant, null);
        Set(variant, "get_IsUnionCase", true);
        Set(variant, "get_Union", definition);
        Set(variant, "get_TypeParameters", ImmutableArray.Create(caseParameter));
        var argument = Named("Error");
        Set(variant, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(argument));
        Set(variant, "TryGetProjectedUnionTypeParameter", (Func<object?[]?, object?>)(arguments =>
        {
            arguments![1] = e;
            return true;
        }));
        var expected = Union("Choice", [t, e], [t, argument], definition);
        Set(definition, "Construct", (Func<object?[]?, object?>)(arguments =>
        {
            Assert.Equal(new TypeSymbol[] { t, argument }, Assert.IsType<TypeSymbol[]>(arguments![0]));
            return expected;
        }));
        Assert.Same(expected, TypeSymbolNormalization.GetBestCommonType([variant]));
    }

    [Fact]
    public void UnionAliasesAndUngenericCasesCollapseToCarrier()
    {
        var carrier = Union("Choice", []);
        var alias = Named("Alias");
        Set(alias, "get_IsAlias", true);
        Set(alias, "get_UnderlyingSymbol", carrier);
        Assert.Same(carrier, TypeSymbolNormalization.GetBestCommonType([alias]));
        var variant = Make<UnionCaseTypeSymbol>(SymbolKind.Type, "First");
        Configure(variant, null);
        Set(variant, "get_IsUnionCase", true);
        Set(variant, "get_Union", carrier);
        Assert.Same(carrier, TypeSymbolNormalization.GetBestCommonType([variant]));
        var nested = Named("First");
        Set(nested, "get_ContainingType", carrier);
        Set(carrier, "get_Variants", ImmutableArray.Create<TypeSymbol>(variant));
        Assert.Same(carrier, TypeSymbolNormalization.GetBestCommonType([nested]));
    }

    [Fact]
    public void ImportedCaseArgumentsProjectByParameterName()
    {
        var t = Parameter("T", 0);
        var e = Parameter("E", 1);
        var definition = Union("Choice", [t, e]);
        var variant = Make<UnionCaseTypeSymbol>(SymbolKind.Type, "Failure");
        Configure(variant, null);
        Set(variant, "get_IsUnionCase", true);
        Set(variant, "get_Union", definition);
        Set(variant, "get_TypeParameters", ImmutableArray.Create(Parameter("E", 0)));
        var argument = Named("Error");
        Set(variant, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(argument));
        var expected = Union("Choice", [t, e], [t, argument], definition);
        Set(definition, "Construct", (Func<object?[]?, object?>)(arguments =>
        {
            Assert.Equal(new TypeSymbol[] { t, argument }, Assert.IsType<TypeSymbol[]>(arguments![0]));
            return expected;
        }));
        Assert.Same(expected, TypeSymbolNormalization.GetBestCommonType([variant]));
    }

    [Fact]
    public void ConflictingConcreteUnionArgumentsUseNominalFallback()
    {
        var t = Parameter("T", 0);
        var definition = Union("Choice", [t]);
        var left = Union("Choice", [t], [Named("First")], definition);
        var right = Union("Choice", [t], [Named("Second")], definition);
        var root = ObjectType();
        Set(left, "get_BaseType", root);
        Set(right, "get_BaseType", root);
        Assert.Same(root, Infer(left, right));
        Assert.Same(root, Infer(left, right, true));
    }

    private static TypeSymbol Infer(TypeSymbol left, TypeSymbol right, bool reverse = false) =>
        TypeSymbolNormalization.GetBestCommonType(reverse ? [right, left] : [left, right]);

    private static NamedTypeSymbol ObjectType()
    {
        var result = Named("Object");
        Set(result, "get_SpecialType", SpecialType.System_Object);
        return result;
    }

    private static NamedTypeSymbol Named(string name, NamedTypeSymbol? baseType = null, TypeKind kind = TypeKind.Class,
        ImmutableArray<NamedTypeSymbol> interfaces = default)
    {
        var result = Make<NamedTypeSymbol>(SymbolKind.Type, name, kind);
        Configure(result, baseType, interfaces);
        Set(result, "get_IsValueType", kind == TypeKind.Struct);
        Set(result, "get_IsReferenceType", kind is TypeKind.Class or TypeKind.Interface);
        return result;
    }

    private static UnionSymbol Union(string name, ImmutableArray<TypeParameterSymbol> parameters,
        ImmutableArray<TypeSymbol> arguments = default, NamedTypeSymbol? definition = null)
    {
        var result = Make<UnionSymbol>(SymbolKind.Type, name, TypeKind.Struct);
        Configure(result, null);
        Set(result, "get_IsUnion", true);
        Set(result, "get_IsGenericType", parameters.Length > 0);
        Set(result, "get_Arity", parameters.Length);
        Set(result, "get_TypeParameters", parameters);
        Set(result, "get_TypeArguments", arguments.IsDefault ? parameters.Cast<TypeSymbol>().ToImmutableArray() : arguments);
        Set(result, "get_OriginalDefinition", definition ?? result);
        Set(result, "get_ConstructedFrom", definition ?? result);
        return result;
    }

    private static void Configure(TypeSymbol type, NamedTypeSymbol? baseType, ImmutableArray<NamedTypeSymbol> interfaces = default)
    {
        Set(type, "get_BaseType", baseType);
        Set(type, "get_Interfaces", interfaces.IsDefault ? ImmutableArray<NamedTypeSymbol>.Empty : interfaces);
        Set(type, "get_AllInterfaces", interfaces.IsDefault ? ImmutableArray<NamedTypeSymbol>.Empty : interfaces);
        Set(type, "get_IsUnion", false);
        Set(type, "get_IsUnionCase", false);
        Set(type, "get_IsNullable", false);
        Set(type, "get_IsValueType", false);
        Set(type, "get_IsReferenceType", true);
        Set(type, "get_IsTupleType", false);
        Set(type, "get_ContainingAssembly", null);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_ConstructedFrom", type);
        Set(type, "get_Arity", 0);
        Set(type, "ToFullyQualifiedMetadataName", type.Name);
    }

    internal interface MappedCase : UnionCaseTypeSymbol, UnionCaseTypeParameterMapping { }
}
