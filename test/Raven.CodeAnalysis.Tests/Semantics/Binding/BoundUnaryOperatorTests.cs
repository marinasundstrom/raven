using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BoundUnaryOperatorTests : CompilationTestBase
{
    [Fact]
    public void TryLookup_BitwiseNotOnIntegralTypes_Succeeds()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);

        var intSuccess = BoundUnaryOperator.TryLookup(compilation, SyntaxKind.TildeToken, intType, out var intOperator);
        var longSuccess = BoundUnaryOperator.TryLookup(compilation, SyntaxKind.TildeToken, longType, out var longOperator);

        Assert.True(intSuccess);
        Assert.Equal(BoundUnaryOperatorKind.BitwiseNot, intOperator.OperatorKind);
        Assert.Equal(intType, intOperator.ResultType);

        Assert.True(longSuccess);
        Assert.Equal(BoundUnaryOperatorKind.BitwiseNot, longOperator.OperatorKind);
        Assert.Equal(longType, longOperator.ResultType);
    }

    [Theory]
    [InlineData(SpecialType.System_SByte)]
    [InlineData(SpecialType.System_Byte)]
    [InlineData(SpecialType.System_Int16)]
    [InlineData(SpecialType.System_UInt16)]
    [InlineData(SpecialType.System_Char)]
    public void TryLookup_BitwiseNotOnSmallIntegralType_PromotesResultToInt32(SpecialType operandSpecialType)
    {
        var compilation = CreateCompilation();
        var operandType = compilation.GetSpecialType(operandSpecialType);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var success = BoundUnaryOperator.TryLookup(
            compilation,
            SyntaxKind.TildeToken,
            operandType,
            out var unaryOperator);

        Assert.True(success);
        Assert.Equal(BoundUnaryOperatorKind.BitwiseNot, unaryOperator.OperatorKind);
        Assert.Equal(operandType, unaryOperator.OperandType);
        Assert.Equal(intType, unaryOperator.ResultType);
    }
}
