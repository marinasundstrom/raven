using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundUnaryOperatorContractTests
{
    [Theory]
    [InlineData(SpecialType.System_Int32, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Int64, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_Boolean, SyntaxKind.ExclamationToken, "LogicalNot")]
    [InlineData(SpecialType.System_Double, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_Int32, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_Int32, SyntaxKind.TildeToken, "BitwiseNot")]
    [InlineData(SpecialType.System_Int64, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Int64, SyntaxKind.TildeToken, "BitwiseNot")]
    [InlineData(SpecialType.System_Double, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Single, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Single, SyntaxKind.MinusToken, "UnaryMinus")]
    public void CanonicalTypesPreserveIdentity(SpecialType specialType, SyntaxKind syntax, string expected)
    {
        var provider = new Provider();
        var operand = provider.GetSpecialType(specialType);
        Assert.True(BoundUnaryOperator.TryLookup(provider, syntax, operand, out var op));
        Assert.Equal(expected, op.OperatorKind.ToString());
        Assert.Same(operand, op.OperandType);
        Assert.Same(operand, op.ResultType);
    }

    [Theory]
    [InlineData(SpecialType.System_SByte, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_SByte, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_SByte, SyntaxKind.TildeToken, "BitwiseNot")]
    [InlineData(SpecialType.System_Byte, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Byte, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_Byte, SyntaxKind.TildeToken, "BitwiseNot")]
    [InlineData(SpecialType.System_Int16, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Int16, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_Int16, SyntaxKind.TildeToken, "BitwiseNot")]
    [InlineData(SpecialType.System_UInt16, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_UInt16, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_UInt16, SyntaxKind.TildeToken, "BitwiseNot")]
    [InlineData(SpecialType.System_Char, SyntaxKind.PlusToken, "UnaryPlus")]
    [InlineData(SpecialType.System_Char, SyntaxKind.MinusToken, "UnaryMinus")]
    [InlineData(SpecialType.System_Char, SyntaxKind.TildeToken, "BitwiseNot")]
    public void SmallIntegralOperatorsPromoteToInt32(SpecialType specialType, SyntaxKind syntax, string expected)
    {
        var provider = new Provider();
        var operand = provider.GetSpecialType(specialType);
        Assert.True(BoundUnaryOperator.TryLookup(provider, syntax, operand, out var op));
        Assert.Equal(expected, op.OperatorKind.ToString());
        Assert.Same(operand, op.OperandType);
        Assert.Same(provider.GetSpecialType(SpecialType.System_Int32), op.ResultType);
    }

    [Fact]
    public void SmallIntegralNegationPromotesAndCheckedVariantPreservesTypes()
    {
        var provider = new Provider();
        var operand = provider.GetSpecialType(SpecialType.System_Byte);
        Assert.True(BoundUnaryOperator.TryLookup(provider, SyntaxKind.MinusToken, operand, out var op));
        var checkedOp = op.WithCheckedNegation();
        Assert.Equal(BoundUnaryOperatorKind.UnaryMinus, op.OperatorKind);
        Assert.Equal(BoundUnaryOperatorKind.CheckedUnaryMinus, checkedOp.OperatorKind);
        Assert.Same(operand, checkedOp.OperandType);
        Assert.Same(provider.GetSpecialType(SpecialType.System_Int32), checkedOp.ResultType);
    }

    [Fact]
    public void ErrorOperandDoesNotResolveCanonicalTypes()
    {
        var provider = new Provider();
        Assert.False(BoundUnaryOperator.TryLookup(provider, SyntaxKind.MinusToken, provider.ErrorTypeSymbol, out var op));
        Assert.Same(provider.ErrorTypeSymbol, op.OperandType);
        Assert.Same(provider.ErrorTypeSymbol, op.ResultType);
        Assert.Equal(0, provider.Lookups);
    }

    [Fact]
    public void UnsupportedOperatorReturnsProviderErrorType()
    {
        var provider = new Provider();
        var operand = provider.GetSpecialType(SpecialType.System_Boolean);
        Assert.False(BoundUnaryOperator.TryLookup(provider, SyntaxKind.PlusToken, operand, out var op));
        Assert.Same(provider.ErrorTypeSymbol, op.ResultType);
    }

    [Fact]
    public void LiteralRepresentationUsesUnderlyingCanonicalType()
    {
        var provider = new Provider();
        var underlying = provider.GetSpecialType(SpecialType.System_Int32);
        var literal = Make<LiteralType>(SymbolKind.Type, "literal");
        Set(literal, "get_UnderlyingType", underlying);
        Assert.True(BoundUnaryOperator.TryLookup(provider, SyntaxKind.PlusToken, literal, out var op));
        Assert.Same(underlying, op.OperandType);
        Assert.Same(underlying, op.ResultType);
    }

    [Fact]
    public void EnumComplementDoesNotResolveCanonicalTypes()
    {
        var provider = new Provider();
        var enumType = Named("Runtime", "Example.Enum");
        Set(enumType, "get_EnumUnderlyingType", Named("Runtime", "System.Int32"));
        Assert.True(BoundUnaryOperator.TryLookup(provider, SyntaxKind.TildeToken, enumType, out var op));
        Assert.Same(enumType, op.OperandType);
        Assert.Same(enumType, op.ResultType);
        Assert.Equal(0, provider.Lookups);
    }

    internal interface LiteralType : TypeSymbol, ILiteralTypeInfo { }

    private sealed class Provider : OperatorTypeProvider
    {
        private readonly Dictionary<SpecialType, NamedTypeSymbol> _types = new();

        public TypeSymbol ErrorTypeSymbol { get; } = Make<TypeSymbol>(SymbolKind.Type, "error", TypeKind.Error);

        public int Lookups { get; private set; }

        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            Lookups++;
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
