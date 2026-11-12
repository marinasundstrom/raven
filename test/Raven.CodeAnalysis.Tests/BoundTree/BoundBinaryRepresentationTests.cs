using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundBinaryRepresentationTests
{
    [Fact]
    public void LiteralOperandsUseTheirUnderlyingType()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        var literal = Make<LiteralInfo>(SymbolKind.Type, "literal", TypeKind.Struct);
        Set(literal, "get_UnderlyingType", integer);
        Set(literal, "get_SpecialType", SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, literal, literal, out var op));
        Assert.Same(integer, op.LeftType);
        Assert.Same(integer, op.RightType);
        Assert.Same(integer, op.ResultType);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void ErrorKindStopsLookupBeforeResolvingTypes(bool errorOnLeft)
    {
        var services = new Services();
        var error = Make<TypeSymbol>(SymbolKind.Type, "error", TypeKind.Error);
        var other = Make<TypeSymbol>(SymbolKind.Type, "other", TypeKind.Struct);
        Assert.False(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken,
            errorOnLeft ? error : other, errorOnLeft ? other : error, out var op));
        Assert.Equal(BinaryOperatorKind.None, op.OperatorKind);
        Assert.Same(services.ErrorTypeSymbol, op.ResultType);
        Assert.Equal(0, services.Lookups);
    }

    [Fact]
    public void NullableReferenceRepresentationUsesUnderlyingReferenceIdentity()
    {
        var services = new Services();
        var underlying = services.GetSpecialType(SpecialType.System_Object);
        var nullable = Wrap(underlying);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.EqualsEqualsToken, nullable, underlying, out var op));
        Assert.Same(underlying, op.LeftType);
        Assert.Same(underlying, op.RightType);
        Assert.Same(services.GetSpecialType(SpecialType.System_Boolean), op.ResultType);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void NullableValueRepresentationSupportsMixedEquality(bool nullableOnLeft)
    {
        var services = new Services();
        var underlying = services.GetSpecialType(SpecialType.System_Int32);
        var nullable = Wrap(underlying);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.EqualsEqualsToken,
            nullableOnLeft ? nullable : underlying, nullableOnLeft ? underlying : nullable, out var op));
        Assert.Equal(BinaryOperatorKind.Equality | BinaryOperatorKind.Lifted, op.OperatorKind);
        Assert.Same(services.GetSpecialType(SpecialType.System_Boolean), op.ResultType);
    }

    private static NullableInfo Wrap(TypeSymbol underlying)
    {
        var result = Make<NullableInfo>(SymbolKind.Type, underlying.Name + "?", TypeKind.Nullable);
        Set(result, "get_UnderlyingType", underlying);
        Set(result, "get_IsNullable", true);
        Set(result, "get_IsReferenceType", underlying.IsReferenceType);
        Set(result, "get_IsValueType", underlying.IsValueType);
        Set(result, "get_BaseType", null);
        Set(result, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(result, "get_Interfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return result;
    }

    internal interface LiteralInfo : TypeSymbol, ILiteralTypeInfo { }
    internal interface NullableInfo : TypeSymbol, INullableTypeIdentity { }

    private sealed class Services : BinaryOperatorServices
    {
        private readonly Dictionary<SpecialType, NamedTypeSymbol> _types = new();
        public TypeSymbol ErrorTypeSymbol { get; } = Make<TypeSymbol>(SymbolKind.Type, "error", TypeKind.Error);
        public int Lookups { get; private set; }
        public bool HasRuntimeSelfContract => false;

        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            Lookups++;
            if (_types.TryGetValue(specialType, out var type))
                return type;
            type = Named("Runtime", specialType.ToString());
            Set(type, "get_SpecialType", specialType);
            Set(type, "get_IsNullable", false);
            Set(type, "get_IsReferenceType", specialType == SpecialType.System_Object);
            Set(type, "get_IsValueType", specialType != SpecialType.System_Object);
            Set(type, "get_TypeKind", specialType == SpecialType.System_Object ? TypeKind.Class : TypeKind.Struct);
            Set(type, "get_EnumUnderlyingType", null);
            Set(type, "get_BaseType", null);
            Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
            Set(type, "get_Interfaces", ImmutableArray<NamedTypeSymbol>.Empty);
            Set(type, "GetMembers", ImmutableArray<Symbol>.Empty);
            _types.Add(specialType, type);
            return type;
        }

        public TypeSymbol CreateNullableType(TypeSymbol type) => Wrap(type);
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
            => throw new InvalidOperationException("Built-in lookup should not request user-defined candidate conversions.");
        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method)
            => throw new InvalidOperationException("Runtime Self is disabled in this fixture.");
    }
}
