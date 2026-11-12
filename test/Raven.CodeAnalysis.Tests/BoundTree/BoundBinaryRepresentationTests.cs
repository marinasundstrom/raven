using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Macros;
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

    [Theory]
    [InlineData(SpecialType.System_Int32, SpecialType.System_Int64, SpecialType.System_Int64)]
    [InlineData(SpecialType.System_Byte, SpecialType.System_Int32, SpecialType.System_Int32)]
    [InlineData(SpecialType.System_Single, SpecialType.System_Double, SpecialType.System_Double)]
    [InlineData(SpecialType.System_Int32, SpecialType.System_Decimal, SpecialType.System_Decimal)]
    public void NumericPromotionUsesCanonicalTypes(SpecialType left, SpecialType right, SpecialType result)
    {
        var services = new Services();
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken,
            services.GetSpecialType(left), services.GetSpecialType(right), out var op));
        Assert.Equal(BinaryOperatorKind.Addition, op.OperatorKind);
        Assert.Same(services.GetSpecialType(result), op.LeftType);
        Assert.Same(op.LeftType, op.RightType);
        Assert.Same(op.LeftType, op.ResultType);
    }

    [Theory]
    [InlineData(SyntaxKind.PlusToken, false)]
    [InlineData(SyntaxKind.EqualsEqualsToken, true)]
    public void NullablePairLiftsArithmeticButKeepsEqualityBoolean(SyntaxKind syntax, bool booleanResult)
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        var nullable = Wrap(integer);
        Assert.True(BoundBinaryOperator.TryLookup(services, syntax, nullable, nullable, out var op));
        Assert.Equal((booleanResult ? BinaryOperatorKind.Equality : BinaryOperatorKind.Addition) | BinaryOperatorKind.Lifted, op.OperatorKind);
        Assert.Same(nullable, op.LeftType);
        Assert.Same(nullable, op.RightType);
        if (booleanResult)
            Assert.Same(services.GetSpecialType(SpecialType.System_Boolean), op.ResultType);
        else
            Assert.Same(integer, Assert.IsAssignableFrom<INullableTypeIdentity>(op.ResultType).UnderlyingType);
    }

    [Fact]
    public void CheckedCopyPreservesOperandAndResultIdentities()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, integer, integer, out var op));
        var copy = op.WithChecked();
        Assert.NotSame(op, copy);
        Assert.Equal(BinaryOperatorKind.Addition, op.OperatorKind);
        Assert.Equal(BinaryOperatorKind.Addition | BinaryOperatorKind.Checked, copy.OperatorKind);
        Assert.Same(op.LeftType, copy.LeftType);
        Assert.Same(op.RightType, copy.RightType);
        Assert.Same(op.ResultType, copy.ResultType);
        Assert.Same(op.MethodSymbol, copy.MethodSymbol);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void UserDefinedCandidatesRequireImplicitStandardConversions(bool implicitConversion)
    {
        var services = new Services { CandidateConversion = new Conversion(isImplicit: implicitConversion) };
        var operand = services.GetSpecialType(SpecialType.System_Object);
        var method = OperatorMethod(operand);
        Set(operand, "GetMembers", ImmutableArray.Create<Symbol>(method));
        Assert.Equal(implicitConversion,
            BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, operand, operand, out var op));
        Assert.Equal(new[] { false, false }, services.ConversionFlags);
        if (implicitConversion)
        {
            Assert.Same(method, op.MethodSymbol);
            Assert.Same(operand, op.ResultType);
        }
        else
            Assert.Same(services.ErrorTypeSymbol, op.ResultType);
    }

    [Fact]
    public void RuntimeSelfUsesTheProjectedMethod()
    {
        var services = new Services { HasRuntimeSelfContract = true };
        var operand = Make<TypeParameterSymbol>(SymbolKind.Type, "T", TypeKind.TypeParameter);
        Set(operand, "get_IsValueType", false);
        Set(operand, "get_IsReferenceType", false);
        Set(operand, "get_IsNullable", false);
        var contract = services.GetSpecialType(SpecialType.System_Object);
        var declaration = OperatorMethod(operand);
        Set(declaration, "get_IsAbstract", true);
        Set(contract, "GetMembers", ImmutableArray.Create<Symbol>(declaration));
        Set(operand, "get_AllInterfaces", ImmutableArray.Create(contract));
        var projected = OperatorMethod(operand);
        services.Projection = projected;
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, operand, operand, out var op));
        Assert.Same(projected, op.MethodSymbol);
        Assert.Same(operand, op.ResultType);
        Assert.Equal(1, services.Projections);
        Assert.Empty(services.ConversionFlags);
    }

    [Theory]
    [InlineData(SyntaxKind.AmpersandAmpersandToken)]
    [InlineData(SyntaxKind.BarBarToken)]
    [InlineData(SyntaxKind.EqualsEqualsToken)]
    public void BooleanOperatorsKeepCanonicalBoolean(SyntaxKind syntax)
    {
        var services = new Services();
        var boolean = services.GetSpecialType(SpecialType.System_Boolean);
        Assert.True(BoundBinaryOperator.TryLookup(services, syntax, boolean, boolean, out var op));
        Assert.Same(boolean, op.ResultType);
        Assert.Same(boolean, op.LeftType);
        Assert.Same(boolean, op.RightType);
    }

    [Theory]
    [InlineData(SyntaxKind.LessThanLessThanToken)]
    [InlineData(SyntaxKind.GreaterThanGreaterThanToken)]
    [InlineData(SyntaxKind.GreaterThanGreaterThanGreaterThanToken)]
    public void ShiftsPromoteSmallOperandsToInt32(SyntaxKind syntax)
    {
        var services = new Services();
        var operand = services.GetSpecialType(SpecialType.System_Byte);
        Assert.True(BoundBinaryOperator.TryLookup(services, syntax, operand, operand, out var op));
        Assert.Same(services.GetSpecialType(SpecialType.System_Int32), op.ResultType);
        Assert.Same(op.ResultType, op.LeftType);
        Assert.Same(op.ResultType, op.RightType);
    }

    [Fact]
    public void PointerAdditionAndSubtractionPreserveTheirDistinctResults()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        var pointer = Make<PointerTypeSymbol>(SymbolKind.Type, "int*", TypeKind.Pointer);
        Set(pointer, "get_PointedAtType", integer);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, pointer, integer, out var addition));
        Assert.Same(pointer, addition.ResultType);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.MinusToken, pointer, pointer, out var subtraction));
        Assert.Same(services.GetSpecialType(SpecialType.System_IntPtr), subtraction.ResultType);
    }

    [Theory]
    [InlineData(SyntaxKind.BarToken, false)]
    [InlineData(SyntaxKind.EqualsEqualsToken, true)]
    public void EnumOperatorsKeepEnumOrBooleanResults(SyntaxKind syntax, bool booleanResult)
    {
        var services = new Services();
        var operand = services.GetSpecialType(SpecialType.System_Object);
        Set(operand, "get_SpecialType", SpecialType.None);
        Set(operand, "get_TypeKind", TypeKind.Enum);
        Set(operand, "get_IsValueType", true);
        Set(operand, "get_IsReferenceType", false);
        Set(operand, "get_EnumUnderlyingType", services.GetSpecialType(SpecialType.System_Int32));
        Assert.True(BoundBinaryOperator.TryLookup(services, syntax, operand, operand, out var op));
        Assert.Same(booleanResult ? services.GetSpecialType(SpecialType.System_Boolean) : operand, op.ResultType);
    }

    [Theory]
    [InlineData(SyntaxKind.PlusToken, false)]
    [InlineData(SyntaxKind.LessThanToken, true)]
    public void NumericTypeParameterConstraintsPreserveArithmeticAndComparisonResults(SyntaxKind syntax, bool booleanResult)
    {
        var services = new Services();
        var operand = Make<TypeParameterSymbol>(SymbolKind.Type, "T", TypeKind.TypeParameter);
        Set(operand, "get_IsValueType", false);
        Set(operand, "get_IsReferenceType", false);
        Set(operand, "get_IsNullable", false);
        Set(operand, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        var global = Make<NamespaceSymbol>(SymbolKind.Namespace, "");
        Set(global, "get_IsGlobalNamespace", true);
        var system = Make<NamespaceSymbol>(SymbolKind.Namespace, "System");
        Set(system, "get_IsGlobalNamespace", false);
        Set(system, "get_ContainingNamespace", global);
        var numerics = Make<NamespaceSymbol>(SymbolKind.Namespace, "Numerics");
        Set(numerics, "get_IsGlobalNamespace", false);
        Set(numerics, "get_ContainingNamespace", system);
        var constraint = Named("Runtime", "INumber");
        Set(constraint, "get_ContainingNamespace", numerics);
        Set(operand, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(constraint));
        Assert.True(BoundBinaryOperator.TryLookup(services, syntax, operand, operand, out var op));
        Assert.Same(booleanResult ? services.GetSpecialType(SpecialType.System_Boolean) : operand, op.ResultType);
        Assert.Same(operand, op.LeftType);
        Assert.Same(operand, op.RightType);
    }

    private static MethodSymbol OperatorMethod(TypeSymbol operand)
    {
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "operand");
        Set(parameter, "get_Type", operand);
        Set(parameter, "get_RefKind", RefKind.None);
        Set(parameter, "get_MacroRole", MacroParameterRole.None);
        var method = Make<MethodSymbol>(SymbolKind.Method, "op_Addition");
        Set(method, "get_IsStatic", true);
        Set(method, "get_IsAbstract", false);
        Set(method, "get_Parameters", ImmutableArray.Create(parameter, parameter));
        Set(method, "get_ReturnType", operand);
        Set(method, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(method, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        return method;
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
        public bool HasRuntimeSelfContract { get; init; }
        public Conversion? CandidateConversion { get; init; }
        public List<bool> ConversionFlags { get; } = [];
        public MethodSymbol? Projection { get; set; }
        public int Projections { get; private set; }

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
        {
            ConversionFlags.Add(includeUserDefined);
            return CandidateConversion ?? throw new InvalidOperationException("Unexpected candidate conversion.");
        }
        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method)
        {
            Projections++;
            return Projection ?? throw new InvalidOperationException("Unexpected runtime Self projection.");
        }
    }
}
