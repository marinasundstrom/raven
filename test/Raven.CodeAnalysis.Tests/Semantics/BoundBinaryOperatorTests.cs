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
}
