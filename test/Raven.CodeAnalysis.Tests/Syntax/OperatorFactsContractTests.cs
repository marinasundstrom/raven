using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax;

[Trait("Target", "Shared")]
public sealed class OperatorFactsContractTests
{
    [Theory]
    [InlineData(SyntaxKind.PlusToken, 1, "op_UnaryPlus", "Unary")]
    [InlineData(SyntaxKind.PlusToken, 2, "op_Addition", "Binary")]
    [InlineData(SyntaxKind.MinusToken, 1, "op_UnaryNegation", "Unary")]
    [InlineData(SyntaxKind.MinusToken, 2, "op_Subtraction", "Binary")]
    [InlineData(SyntaxKind.PlusPlusToken, 0, "op_IncrementAssignment", "InstanceIncrement")]
    [InlineData(SyntaxKind.PlusPlusToken, 1, "op_Increment", "Unary")]
    [InlineData(SyntaxKind.MinusMinusToken, 0, "op_DecrementAssignment", "InstanceIncrement")]
    [InlineData(SyntaxKind.TildeToken, 1, "op_OnesComplement", "Unary")]
    [InlineData(SyntaxKind.ExclamationToken, 1, "op_LogicalNot", "Unary")]
    [InlineData(SyntaxKind.TrueKeyword, 1, "op_True", "Unary")]
    [InlineData(SyntaxKind.PlusEqualsToken, 1, "op_AdditionAssignment", "CompoundAssignment")]
    [InlineData(SyntaxKind.StarEqualsToken, 1, "op_MultiplicationAssignment", "CompoundAssignment")]
    [InlineData(SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken, 1, "op_UnsignedRightShiftAssignment", "CompoundAssignment")]
    [InlineData(SyntaxKind.GreaterThanGreaterThanGreaterThanToken, 2, "op_UnsignedRightShift", "Binary")]
    [InlineData(SyntaxKind.AndToken, 2, "op_LogicalAnd", "Binary")]
    [InlineData(SyntaxKind.AmpersandAmpersandToken, 2, "op_LogicalAnd", "Binary")]
    [InlineData(SyntaxKind.OrToken, 2, "op_LogicalOr", "Binary")]
    [InlineData(SyntaxKind.BarBarToken, 2, "op_LogicalOr", "Binary")]
    public void LookupPreservesMetadataAndArity(SyntaxKind token, int count, string metadata, string arity)
    {
        Assert.True(OperatorFacts.TryGetUserDefinedOperatorInfo(token, count, out var info));
        Assert.Equal(metadata, info.MetadataName);
        Assert.Equal(arity, info.Arity.ToString());
    }

    [Theory]
    [InlineData(SyntaxKind.PlusToken, 0)]
    [InlineData(SyntaxKind.PlusToken, 3)]
    [InlineData(SyntaxKind.PlusEqualsToken, 2)]
    [InlineData(SyntaxKind.PlusPlusToken, 2)]
    [InlineData(SyntaxKind.ExclamationToken, 2)]
    [InlineData(SyntaxKind.IdentifierToken, 1)]
    public void InvalidLookupReturnsDefaultInfo(SyntaxKind token, int count)
    {
        Assert.False(OperatorFacts.TryGetUserDefinedOperatorInfo(token, count, out var info));
        Assert.Equal(default, info);
    }

    [Theory]
    [InlineData("op_Addition", "op_CheckedAddition")]
    [InlineData("op_MultiplicationAssignment", "op_CheckedMultiplicationAssignment")]
    [InlineData("op_Explicit", "op_CheckedExplicit")]
    [InlineData("op_Implicit", null)]
    [InlineData("op_Equality", null)]
    public void CheckedNamesAreRestrictedToSupportedOperators(string name, string? expected)
        => Assert.Equal(expected, OperatorFacts.GetCheckedMetadataName(name));

    [Fact]
    public void DeclarationOverloadsPreserveCheckedNames()
    {
        var tree = SyntaxTree.ParseText("""
            class Box {
                static func checked +(left: Box, right: Box) -> Box => left
                static func checked explicit(value: Box) -> int => 1
            }
            """);
        Assert.Empty(tree.GetDiagnostics());
        var root = tree.GetRoot();
        Assert.True(OperatorFacts.TryGetUserDefinedOperatorInfo(root.DescendantNodes().OfType<OperatorDeclarationSyntax>().Single(), out var info));
        Assert.Equal("op_CheckedAddition", info.MetadataName);
        Assert.True(OperatorFacts.TryGetConversionOperatorMetadataName(root.DescendantNodes().OfType<ConversionOperatorDeclarationSyntax>().Single(), out var name));
        Assert.Equal("op_CheckedExplicit", name);
    }

    [Fact]
    public void ConversionNamesAndDisplayTextPreserveFallbacks()
    {
        Assert.True(OperatorFacts.TryGetConversionOperatorMetadataName(SyntaxKind.ImplicitKeyword, out var name));
        Assert.Equal("op_Implicit", name);
        Assert.False(OperatorFacts.TryGetConversionOperatorMetadataName(SyntaxKind.PlusToken, out name));
        Assert.Equal(string.Empty, name);
        Assert.Equal("+", OperatorFacts.GetDisplayText(SyntaxKind.PlusToken));
        Assert.Equal("implicit operator", OperatorFacts.GetConversionOperatorDisplayText(SyntaxKind.ImplicitKeyword));
        Assert.Equal("1 or 2", OperatorFacts.GetExpectedParameterCountDescription(SyntaxKind.PlusToken));
        Assert.Equal("0 or 1", OperatorFacts.GetExpectedParameterCountDescription(SyntaxKind.PlusPlusToken));
        Assert.Equal("1", OperatorFacts.GetExpectedParameterCountDescription(SyntaxKind.PlusEqualsToken));
        Assert.Equal("2", OperatorFacts.GetExpectedParameterCountDescription(SyntaxKind.StarToken));
    }

    [Fact]
    public void OperatorInfoHasRecordValueSemantics()
    {
        var first = new UserDefinedOperatorInfo("op_Addition", OperatorArity.Binary);
        var same = new UserDefinedOperatorInfo("op_Addition", OperatorArity.Binary);
        Assert.True(first == same);
        Assert.Equal(first.GetHashCode(), same.GetHashCode());
        var (name, arity) = first;
        Assert.Equal("op_Addition", name);
        Assert.Equal(OperatorArity.Binary, arity);
        Assert.Equal("UserDefinedOperatorInfo { MetadataName = op_Addition, Arity = Binary }", first.ToString());
        var changed = first with { MetadataName = "op_CheckedAddition" };
        Assert.True(first != changed);
        Assert.Equal(OperatorArity.Binary, changed.Arity);
        Assert.Equal("op_Addition", first.MetadataName);
    }
}
