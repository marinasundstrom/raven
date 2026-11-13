using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class AttributeDataFactoryContractTests
{
    [Fact]
    public void LiteralsConversionsNullAndTypeOfPreserveTypesAndValues()
    {
        var type = Named("Runtime", "Example.Int");
        var target = Named("Runtime", "Example.Target");
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 42, type, target);
        Assert.True(AttributeDataFactory.TryCreateTypedConstant(literal, out var constant));
        Assert.Same(target, constant.Type);
        Assert.Equal(42, constant.Value);
        Assert.Equal(TypedConstantKind.Primitive, constant.Kind);
        var conversion = new BoundConversionExpression(literal, type, new Conversion(isImplicit: true));
        Assert.Same(type, AttributeDataFactory.CreateTypedConstant(conversion).Type);
        Assert.Equal(42, AttributeDataFactory.CreateTypedConstant(conversion).Value);
        var nil = AttributeDataFactory.CreateTypedConstant(new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null, type));
        Assert.True(nil.IsNull);
        Assert.Same(type, nil.Type);
        var typeOf = AttributeDataFactory.CreateTypedConstant(new BoundTypeOfExpression(target, type));
        Assert.Equal(TypedConstantKind.Type, typeOf.Kind);
        Assert.Same(target, typeOf.Value);
        Assert.Same(type, typeOf.Type);
    }

    [Fact]
    public void ArraysPreserveOrderAndRejectInvalidElements()
    {
        var type = Named("Runtime", "Example.Int");
        var array = Make<ArrayTypeSymbol>(SymbolKind.Type, "Array", TypeKind.Array);
        var value = AttributeDataFactory.CreateTypedConstant(new BoundCollectionExpression(array, [Literal(type, 2), Literal(type, 1)]));
        Assert.Equal(TypedConstantKind.Array, value.Kind);
        Assert.Same(array, value.Type);
        Assert.Equal(new object?[] { 2, 1 }, value.Values.Select(item => item.Value));
        Assert.Empty(AttributeDataFactory.CreateTypedConstant(new BoundEmptyCollectionExpression(array)).Values);
        Assert.False(AttributeDataFactory.TryCreateTypedConstant(new BoundCollectionExpression(array, [Literal(type, 1), new BoundErrorExpression(type)]), out var error));
        Assert.Equal(TypedConstantKind.Error, error.Kind);
        Assert.Same(array, error.Type);
        Assert.Equal(TypedConstantKind.Error, AttributeDataFactory.CreateTypedConstant(new BoundUnitExpression(type)).Kind);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConstantFieldsWorkThroughBothAccessShapes(bool enumeration)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, "Value", enumeration ? TypeKind.Enum : TypeKind.Struct);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", type);
        Set(field, "get_IsConst", !enumeration);
        Set(field, "get_ContainingType", type);
        Set(field, "GetConstantValue", 7);
        foreach (var expression in new BoundExpression[] { new BoundFieldAccess(field), new BoundMemberAccessExpression(null, field) })
        {
            var constant = AttributeDataFactory.CreateTypedConstant(expression);
            Assert.Equal(enumeration ? TypedConstantKind.Enum : TypedConstantKind.Primitive, constant.Kind);
            Assert.Equal(7, constant.Value);
            Assert.Same(type, constant.Type);
        }
    }

    [Theory]
    [InlineData(SyntaxKind.AmpersandToken, 2)]
    [InlineData(SyntaxKind.BarToken, 7)]
    [InlineData(SyntaxKind.CaretToken, 5)]
    public void BinaryBitwiseConstantsRetainInt32Storage(SyntaxKind token, int expected)
    {
        var provider = new Provider();
        var type = provider.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(provider, token, type, type, out var op));
        var constant = AttributeDataFactory.CreateTypedConstant(new BoundBinaryExpression(Literal(type, 6), op.WithChecked(), Literal(type, 3)));
        Assert.Equal(expected, Assert.IsType<int>(constant.Value));
        Assert.Same(type, constant.Type);
    }

    [Fact]
    public void UnsupportedBinaryAndNullOperandsProduceErrors()
    {
        var provider = new Provider();
        var type = provider.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(provider, SyntaxKind.PlusToken, type, type, out var op));
        Assert.False(AttributeDataFactory.TryCreateTypedConstant(new BoundBinaryExpression(Literal(type, 1), op, Literal(type, 2)), out var error));
        Assert.Equal(TypedConstantKind.Error, error.Kind);
        Assert.True(BoundBinaryOperator.TryLookup(provider, SyntaxKind.BarToken, type, type, out op));
        Assert.False(AttributeDataFactory.TryCreateTypedConstant(new BoundBinaryExpression(
            new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null, type), op, Literal(type, 2)), out error));
        Assert.Equal(TypedConstantKind.Error, error.Kind);
    }

    [Fact]
    public void AttributeCreationPreservesConstructorArgumentsAndNamedMemberTypes()
    {
        var owner = Named("Runtime", "Example.Attribute");
        var type = Named("Runtime", "Example.Int");
        var ctor = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        Set(ctor, "get_ContainingType", owner);
        var property = Make<PropertySymbol>(SymbolKind.Property, "Property");
        Set(property, "get_Type", owner);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Field");
        Set(field, "get_Type", type);
        var syntax = SyntaxTree.ParseText("[Example]\nclass C {}").GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        var creation = new BoundObjectCreationExpression(ctor, [Literal(type, 4)], initializer: new BoundObjectInitializer([
            new BoundObjectInitializerAssignmentEntry(property, Literal(type, 5)),
            new BoundObjectInitializerExpressionEntry(Literal(type, 99)),
            new BoundObjectInitializerAssignmentEntry(field, Literal(type, 6))]));
        var data = AttributeDataFactory.Create(creation, syntax)!;
        Assert.Same(owner, data.AttributeClass);
        Assert.Same(ctor, data.AttributeConstructor);
        Assert.Equal(4, Assert.Single(data.ConstructorArguments).Value);
        Assert.Equal(new[] { "Property", "Field" }, data.NamedArguments.Select(item => item.Key));
        Assert.Same(owner, data.NamedArguments[0].Value.Type);
        Assert.Equal(5, data.NamedArguments[0].Value.Value);
        Assert.Same(type, data.NamedArguments[1].Value.Type);
        Assert.Equal(syntax.Span, data.ApplicationSyntaxReference!.Span);
        Assert.Null(AttributeDataFactory.Create(null, syntax));
        Assert.Null(AttributeDataFactory.Create(Literal(type, 1), syntax));
    }

    public static IEnumerable<object[]> BitwiseStorageCases()
    {
        (SpecialType Type, object High, object Combined)[] types =
        [
            (SpecialType.System_SByte, sbyte.MinValue, (sbyte)(sbyte.MinValue + 1)),
            (SpecialType.System_Byte, (byte)128, (byte)129),
            (SpecialType.System_Int16, short.MinValue, (short)(short.MinValue + 1)),
            (SpecialType.System_UInt16, (ushort)32768, (ushort)32769),
            (SpecialType.System_Int32, int.MinValue, int.MinValue + 1),
            (SpecialType.System_UInt32, 0x80000000U, 0x80000001U),
            (SpecialType.System_Int64, long.MinValue, long.MinValue + 1),
            (SpecialType.System_UInt64, 0x8000000000000000UL, 0x8000000000000001UL)
        ];
        foreach (var (type, high, combined) in types)
        {
            foreach (var token in new[] { SyntaxKind.AmpersandToken, SyntaxKind.BarToken, SyntaxKind.CaretToken })
            {
                var expected = token == SyntaxKind.AmpersandToken ? Convert.ChangeType(0, high.GetType()) : combined;
                yield return [type, high, Convert.ChangeType(1, high.GetType()), token, expected, true];
                if (type is SpecialType.System_Int32 or SpecialType.System_UInt32 or SpecialType.System_Int64 or SpecialType.System_UInt64)
                    yield return [type, high, Convert.ChangeType(1, high.GetType()), token, expected, false];
            }
        }
        yield return [SpecialType.System_Boolean, true, false, SyntaxKind.AmpersandToken, false, false];
        yield return [SpecialType.System_Boolean, true, false, SyntaxKind.BarToken, true, false];
        yield return [SpecialType.System_Boolean, true, false, SyntaxKind.CaretToken, true, false];
    }

    [Theory]
    [MemberData(nameof(BitwiseStorageCases))]
    public void BitwiseConstantsPreserveDeclaredStorageAtBoundaries(SpecialType storageType, object left,
        object right, SyntaxKind token, object expected, bool enumeration)
    {
        var provider = new Provider();
        var type = provider.GetSpecialType(storageType);
        if (enumeration)
        {
            var enumType = Make<NamedTypeSymbol>(SymbolKind.Type, "Flags", TypeKind.Enum);
            Set(enumType, "get_EnumUnderlyingType", type);
            Set(enumType, "Equals", (Func<object?[]?, object?>)(arguments => ReferenceEquals(enumType, arguments![0])));
            type = enumType;
        }
        Assert.True(BoundBinaryOperator.TryLookup(provider, token, type, type, out var op));
        var expression = new BoundBinaryExpression(
            new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, left, type), op.WithChecked(),
            new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, right, type));
        Assert.True(AttributeDataFactory.TryCreateTypedConstant(expression, out var result));
        Assert.Equal(expected.GetType(), result.Value!.GetType());
        Assert.Equal(expected, result.Value);
        Assert.Equal(enumeration ? TypedConstantKind.Enum : TypedConstantKind.Primitive, result.Kind);
        Assert.Same(type, result.Type);
    }

    private static BoundLiteralExpression Literal(TypeSymbol type, int value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private sealed class Provider : BinaryOperatorServices
    {
        private readonly Dictionary<SpecialType, NamedTypeSymbol> _types = new();
        public TypeSymbol ErrorTypeSymbol { get; } = Make<TypeSymbol>(SymbolKind.Type, "error", TypeKind.Error);
        public bool HasRuntimeSelfContract => false;
        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method) => null;
        public TypeSymbol CreateNullableType(TypeSymbol type) => throw new NotSupportedException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
            => new(isIdentity: ReferenceEquals(source, destination));
        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            if (!_types.TryGetValue(specialType, out var type))
            {
                type = Named("Runtime", specialType.ToString());
                Set(type, "get_SpecialType", specialType);
                Set(type, "get_EnumUnderlyingType", null);
                _types.Add(specialType, type);
            }
            return type;
        }
    }
}
