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
        var source = "let x = 1 + true";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes, diagnostic.Descriptor);
    }

    [Fact]
    public void Bind_InvalidOperator_UsesOperatorText()
    {
        var source = "let x = true < 1";
        var (compilation, _) = CreateCompilation(source);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());

        Assert.Equal("Operator '<' cannot be applied to operands of type 'bool' and 'int'", diagnostic.GetMessage());
    }

    [Fact]
    public void TryLookup_BoolBitwiseOperators_Succeeds()
    {
        var compilation = CreateCompilation();
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        var andSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.AmpersandToken, boolType, boolType, out var and);
        var orSuccess = BoundBinaryOperator.TryLookup(compilation, SyntaxKind.BarToken, boolType, boolType, out var or);

        Assert.True(andSuccess);
        Assert.Equal(BinaryOperatorKind.BitwiseAnd, and.OperatorKind);
        Assert.Equal(boolType, and.ResultType);

        Assert.True(orSuccess);
        Assert.Equal(BinaryOperatorKind.BitwiseOr, or.OperatorKind);
        Assert.Equal(boolType, or.ResultType);
    }

    [Fact]
    public void Bind_BoolBitwiseCompoundAssignment_NoDiagnostics()
    {
        var source = "var b = true\nb &= false\nlet c = b | false";
        var (compilation, _) = CreateCompilation(source);

        Assert.Empty(compilation.GetDiagnostics());
    }
}
