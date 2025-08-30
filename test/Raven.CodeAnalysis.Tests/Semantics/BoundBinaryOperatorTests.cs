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
}
