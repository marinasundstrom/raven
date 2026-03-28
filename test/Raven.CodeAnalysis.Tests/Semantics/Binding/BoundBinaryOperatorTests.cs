using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BoundBinaryOperatorTests : CompilationTestBase
{
    [Fact]
    public void TryLookup_InvalidOperator_ReturnsFalse()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        var success = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, intType, boolType, out var op);

        Assert.False(success);
        Assert.Equal(BinaryOperatorKind.None, op.OperatorKind);
    }

    [Fact]
    public void Bind_InvalidOperator_ReportsDiagnostic()
    {
        var source = "val x = 1 + true";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes, diagnostic.Descriptor);
    }

    [Fact]
    public void Bind_InvalidOperator_UsesOperatorText()
    {
        var source = "val x = true < 1";
        var (compilation, _) = CreateCompilation(source);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());

        Assert.Equal("'<' is not supported for 'bool' and 'int'", diagnostic.GetMessage().TrimEnd('.'));
    }

    [Fact]
    public void TryLookup_BoolBitwiseOperators_Succeeds()
    {
        var compilation = CreateCompilation();
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        var andSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.AmpersandToken, boolType, boolType, out var and);
        var orSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.BarToken, boolType, boolType, out var or);
        var xorSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.CaretToken, boolType, boolType, out var xor);

        Assert.True(andSuccess);
        Assert.Equal(BinaryOperatorKind.BitwiseAnd, and.OperatorKind);
        Assert.Equal(boolType, and.ResultType);

        Assert.True(orSuccess);
        Assert.Equal(BinaryOperatorKind.BitwiseOr, or.OperatorKind);
        Assert.Equal(boolType, or.ResultType);

        Assert.True(xorSuccess);
        Assert.Equal(BinaryOperatorKind.BitwiseXor, xor.OperatorKind);
        Assert.Equal(boolType, xor.ResultType);
    }

    [Fact]
    public void TryLookup_ShiftOperators_Succeeds()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);

        var intLeftSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.LessThanLessThanToken, intType, intType, out var intLeft);
        var intRightSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.GreaterThanGreaterThanToken, intType, intType, out var intRight);
        var longLeftSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.LessThanLessThanToken, longType, intType, out var longLeft);
        var longRightSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.GreaterThanGreaterThanToken, longType, intType, out var longRight);

        Assert.True(intLeftSuccess);
        Assert.Equal(BinaryOperatorKind.ShiftLeft, intLeft.OperatorKind);
        Assert.Equal(intType, intLeft.ResultType);

        Assert.True(intRightSuccess);
        Assert.Equal(BinaryOperatorKind.ShiftRight, intRight.OperatorKind);
        Assert.Equal(intType, intRight.ResultType);

        Assert.True(longLeftSuccess);
        Assert.Equal(BinaryOperatorKind.ShiftLeft, longLeft.OperatorKind);
        Assert.Equal(longType, longLeft.ResultType);

        Assert.True(longRightSuccess);
        Assert.Equal(BinaryOperatorKind.ShiftRight, longRight.OperatorKind);
        Assert.Equal(longType, longRight.ResultType);
    }

    [Fact]
    public void Bind_BoolBitwiseCompoundAssignment_NoDiagnostics()
    {
        var source = "var b = true\nb &= false\nlet c = b | false";
        var (compilation, _) = CreateCompilation(source);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(SpecialType.System_Single)]
    [InlineData(SpecialType.System_Double)]
    [InlineData(SpecialType.System_Decimal)]
    [InlineData(SpecialType.System_Int32)]
    [InlineData(SpecialType.System_UInt32)]
    [InlineData(SpecialType.System_Int64)]
    [InlineData(SpecialType.System_UInt64)]
    public void TryLookup_ArithmeticOperators_SucceedForSupportedPredefinedNumericTypes(SpecialType specialType)
    {
        var compilation = CreateCompilation();
        var type = compilation.GetSpecialType(specialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, type, type, out var add));
        Assert.Equal(BinaryOperatorKind.Addition, add.OperatorKind);
        Assert.Equal(specialType, add.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.StarToken, type, type, out var multiply));
        Assert.Equal(BinaryOperatorKind.Multiplication, multiply.OperatorKind);
        Assert.Equal(specialType, multiply.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.LessThanToken, type, type, out var lessThan));
        Assert.Equal(BinaryOperatorKind.LessThan, lessThan.OperatorKind);
        Assert.Equal(SpecialType.System_Boolean, lessThan.ResultType.SpecialType);
    }

    [Fact]
    public void TryLookup_SmallIntegralArithmetic_PromotesToInt()
    {
        var compilation = CreateCompilation();
        var sbyteType = compilation.GetSpecialType(SpecialType.System_SByte);
        var byteType = compilation.GetSpecialType(SpecialType.System_Byte);
        var shortType = compilation.GetSpecialType(SpecialType.System_Int16);
        var ushortType = compilation.GetSpecialType(SpecialType.System_UInt16);
        var charType = compilation.GetSpecialType(SpecialType.System_Char);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, sbyteType, sbyteType, out var sbyteAdd));
        Assert.Equal(SpecialType.System_Int32, sbyteAdd.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, byteType, byteType, out var byteAdd));
        Assert.Equal(SpecialType.System_Int32, byteAdd.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, shortType, shortType, out var shortAdd));
        Assert.Equal(SpecialType.System_Int32, shortAdd.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, ushortType, ushortType, out var ushortAdd));
        Assert.Equal(SpecialType.System_Int32, ushortAdd.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, charType, charType, out var charAdd));
        Assert.Equal(SpecialType.System_Int32, charAdd.ResultType.SpecialType);
    }

    [Fact]
    public void TryLookup_UIntMixedWithInt_PromotesToLong()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var uintType = compilation.GetSpecialType(SpecialType.System_UInt32);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, uintType, intType, out var add));
        Assert.Equal(SpecialType.System_Int64, add.LeftType.SpecialType);
        Assert.Equal(SpecialType.System_Int64, add.RightType.SpecialType);
        Assert.Equal(SpecialType.System_Int64, add.ResultType.SpecialType);
    }

    [Fact]
    public void TryLookup_UlongMixedWithSignedIntegral_IsRejected()
    {
        var compilation = CreateCompilation();
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);
        var ulongType = compilation.GetSpecialType(SpecialType.System_UInt64);

        var success = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.PlusToken, ulongType, longType, out var op);

        Assert.False(success);
        Assert.Equal(BinaryOperatorKind.None, op.OperatorKind);
    }

    [Fact]
    public void TryLookup_ShiftOperators_SupportUnsignedAndPromotedSmallIntegrals()
    {
        var compilation = CreateCompilation();
        var byteType = compilation.GetSpecialType(SpecialType.System_Byte);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var uintType = compilation.GetSpecialType(SpecialType.System_UInt32);
        var ulongType = compilation.GetSpecialType(SpecialType.System_UInt64);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.LessThanLessThanToken, byteType, byteType, out var byteShift));
        Assert.Equal(SpecialType.System_Int32, byteShift.LeftType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, byteShift.RightType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, byteShift.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.LessThanLessThanToken, uintType, intType, out var uintShift));
        Assert.Equal(SpecialType.System_UInt32, uintShift.ResultType.SpecialType);

        Assert.True(BoundBinaryOperator.TryLookup(compilation, SyntaxKind.GreaterThanGreaterThanToken, ulongType, intType, out var ulongShift));
        Assert.Equal(SpecialType.System_UInt64, ulongShift.ResultType.SpecialType);
    }
}
