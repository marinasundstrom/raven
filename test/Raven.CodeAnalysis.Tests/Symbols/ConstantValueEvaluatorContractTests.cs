using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ConstantValueEvaluatorContractTests
{
    [Theory]
    [InlineData("42", true, 42)]
    [InlineData("(-42)", true, -42)]
    [InlineData("+42", true, 42)]
    [InlineData("!true", true, false)]
    [InlineData("false && missing", true, false)]
    [InlineData("true || missing", true, true)]
    [InlineData("true && false", true, false)]
    [InlineData("false || true", true, true)]
    [InlineData("42 == 42", true, true)]
    [InlineData("42 != 41", true, true)]
    [InlineData("null", true, null)]
    [InlineData("null == null", true, true)]
    [InlineData("\"hello\"", true, "hello")]
    [InlineData("missing", false, null)]
    [InlineData("1 + 2", false, null)]
    [InlineData("true && missing", false, null)]
    [InlineData("1 == true", false, null)]
    public void EvaluatesSupportedSyntaxAndShortCircuits(string source, bool success, object? expected)
    {
        Assert.Equal(success, ConstantValueEvaluator.TryEvaluate(SyntaxFactory.ParseExpression(source), out var result));
        Assert.Equal(expected, result);
    }

    [Theory]
    [InlineData(SpecialType.System_Int32, "2147483647", true, 2147483647)]
    [InlineData(SpecialType.System_Int32, "2147483648", false, null)]
    [InlineData(SpecialType.System_Boolean, "true", true, true)]
    [InlineData(SpecialType.System_Char, "x", true, 'x')]
    [InlineData(SpecialType.System_Char, "xx", false, null)]
    [InlineData(SpecialType.System_Double, "1.25", true, 1.25)]
    [InlineData(SpecialType.System_String, "raw", true, "raw")]
    [InlineData(SpecialType.None, "42", false, null)]
    public void ExternalParsingPreservesInvariantTypes(SpecialType special, string text, bool success, object? expected)
    {
        Assert.Equal(success, ConstantValueEvaluator.TryParseExternal(Type(special), text, out var result));
        Assert.Equal(expected, result);
    }

    [Theory]
    [InlineData(SpecialType.System_Byte, 255, true, (byte)255)]
    [InlineData(SpecialType.System_Byte, 256, false, null)]
    [InlineData(SpecialType.System_Byte, -1, false, null)]
    [InlineData(SpecialType.System_SByte, -128, true, (sbyte)-128)]
    [InlineData(SpecialType.System_SByte, -129, false, null)]
    [InlineData(SpecialType.System_Int16, 32767, true, (short)32767)]
    [InlineData(SpecialType.System_UInt16, 65535, true, (ushort)65535)]
    [InlineData(SpecialType.System_Int32, 2147483648L, false, null)]
    [InlineData(SpecialType.System_UInt32, 42, true, 42U)]
    [InlineData(SpecialType.System_Int64, 42, true, 42L)]
    [InlineData(SpecialType.System_UInt64, 42, true, 42UL)]
    [InlineData(SpecialType.System_Single, 42, true, 42F)]
    [InlineData(SpecialType.System_Double, 42, true, 42D)]
    [InlineData(SpecialType.System_Boolean, 1, false, null)]
    [InlineData(SpecialType.System_Char, 65, true, 'A')]
    [InlineData(SpecialType.System_String, 42, false, null)]
    [InlineData(SpecialType.System_Int32, null, false, null)]
    [InlineData(SpecialType.System_String, null, true, null)]
    public void ConversionChecksRangesAndRetainsBoxedType(SpecialType special, object? input, bool success, object? expected)
    {
        Assert.Equal(success, ConstantValueEvaluator.TryConvert(Type(special), input, out var result));
        Assert.Equal(expected, result);
        Assert.Equal(expected?.GetType(), result?.GetType());
    }

    [Fact]
    public void DecimalAndDateTimeRetainTheirValues()
    {
        Assert.True(ConstantValueEvaluator.TryConvert(Type(SpecialType.System_Decimal), 1.25D, out var value));
        Assert.Equal(1.25M, Assert.IsType<decimal>(value));
        var date = new DateTime(2026, 1, 2);
        Assert.True(ConstantValueEvaluator.TryConvert(Type(SpecialType.System_DateTime), date, out value));
        Assert.Equal(date, Assert.IsType<DateTime>(value));
        Assert.True(ConstantValueEvaluator.TryParseExternal(Type(SpecialType.System_Decimal), "1.25", out value));
        Assert.Equal(1.25M, Assert.IsType<decimal>(value));
    }

    [Fact]
    public void EnumUsesUnderlyingRangeAndErrorTypesFail()
    {
        var enumeration = Type(SpecialType.None);
        MetadataSymbolFixture.Set(enumeration, "get_TypeKind", TypeKind.Enum);
        MetadataSymbolFixture.Set(enumeration, "get_EnumUnderlyingType", Type(SpecialType.System_Byte));
        Assert.True(ConstantValueEvaluator.TryConvert(enumeration, 255, out var value));
        Assert.Equal((byte)255, value);
        Assert.False(ConstantValueEvaluator.TryConvert(enumeration, 256, out value));
        Assert.Null(value);
        Assert.True(ConstantValueEvaluator.TryParseExternal(enumeration, "255", out value));
        Assert.Equal((byte)255, value);
        MetadataSymbolFixture.Set(enumeration, "get_TypeKind", TypeKind.Error);
        Assert.False(ConstantValueEvaluator.TryConvert(enumeration, null, out value));
        Assert.Null(value);
    }

    [Theory]
    [InlineData(double.NaN)]
    [InlineData(double.PositiveInfinity)]
    [InlineData(double.NegativeInfinity)]
    [InlineData(double.MaxValue)]
    [InlineData(float.NaN)]
    [InlineData(float.PositiveInfinity)]
    [InlineData(float.MaxValue)]
    public void UnrepresentableDecimalConversionFailsWithoutThrowing(object input)
    {
        Assert.False(ConstantValueEvaluator.TryConvert(Type(SpecialType.System_Decimal), input, out var value));
        Assert.Null(value);
    }

    [Fact]
    public void NullableLiteralAndAliasWrappersPreserveUnderlyingConversions()
    {
        var primitive = Type(SpecialType.System_Int32);
        MetadataSymbolFixture.Set(primitive, "get_BaseType", null);
        var nullable = new NullableTypeSymbol(primitive, null, null, null, [], NullableAbiProjection.AnnotatedUnderlyingType);
        Assert.True(ConstantValueEvaluator.TryConvert(nullable, null, out var result));
        Assert.Null(result);
        Assert.True(ConstantValueEvaluator.TryConvert(nullable, 42, out result));
        Assert.Equal(42, result);
        Assert.True(ConstantValueEvaluator.TryParseExternal(nullable, "42", out result));
        Assert.Equal(42, result);
        var assembly = MetadataSymbolFixture.Make<AssemblySymbol>(SymbolKind.Assembly, "Test");
        var literal = new LiteralTypeSymbol(primitive, 42, assembly);
        var alias = Type(SpecialType.None);
        MetadataSymbolFixture.Set(alias, "get_IsAlias", true);
        MetadataSymbolFixture.Set(alias, "get_UnderlyingSymbol", literal);
        Assert.True(ConstantValueEvaluator.TryConvert(alias, 42L, out result));
        Assert.Equal(42, result);
        Assert.True(ConstantValueEvaluator.TryParseExternal(alias, "42", out result));
        Assert.Equal(42, result);
    }

    private static NamedTypeSymbol Type(SpecialType special)
    {
        var type = MetadataSymbolFixture.Named("Test", "Example.Value");
        MetadataSymbolFixture.Set(type, "get_SpecialType", special);
        MetadataSymbolFixture.Set(type, "get_IsValueType", special is not (SpecialType.System_String or SpecialType.System_Object));
        return type;
    }
}
