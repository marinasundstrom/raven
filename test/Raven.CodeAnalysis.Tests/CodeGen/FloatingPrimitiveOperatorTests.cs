using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class FloatingPrimitiveOperatorTests
{
    [Theory]
    [InlineData(SpecialType.System_Single)]
    [InlineData(SpecialType.System_Double)]
    public void FloatingUnaryOperatorsArePredefined(SpecialType primitive)
    {
        var compilation = Compilation.Create("FloatingUnary", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var type = compilation.GetSpecialType(primitive);
        foreach (var syntax in new[] { SyntaxKind.PlusToken, SyntaxKind.MinusToken })
        {
            Assert.True(BoundUnaryOperator.TryLookup(compilation, syntax, type, out var op));
            Assert.Same(type, op.OperandType);
            Assert.Same(type, op.ResultType);
            Assert.Equal(syntax == SyntaxKind.PlusToken ? BoundUnaryOperatorKind.UnaryPlus : BoundUnaryOperatorKind.UnaryMinus, op.OperatorKind);
        }
        Assert.False(BoundUnaryOperator.TryLookup(compilation, SyntaxKind.TildeToken, type, out _));
    }
}
