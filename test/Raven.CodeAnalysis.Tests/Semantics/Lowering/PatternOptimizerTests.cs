using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests.Semantics.Lowering;

public sealed class PatternOptimizerTests
{
    [Fact]
    public void Rewrite_SimplifiesTrivialPatternAlgebra()
    {
        var compilation = Compilation.Create(
            "pattern_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var discard = new BoundDiscardPattern(booleanType);
        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType);
        var constant = new BoundConstantPattern(literal);

        Assert.Same(
            constant,
            PatternOptimizer.Rewrite(new BoundAndPattern(discard, constant)));
        Assert.Same(
            constant,
            PatternOptimizer.Rewrite(new BoundAndPattern(constant, discard)));
        Assert.Same(
            discard,
            PatternOptimizer.Rewrite(new BoundOrPattern(discard, constant)));
        Assert.Same(
            constant,
            PatternOptimizer.Rewrite(new BoundNotPattern(new BoundNotPattern(constant))));
    }

    [Fact]
    public void Rewrite_DoesNotDropLeftSideOfOrPattern()
    {
        var compilation = Compilation.Create(
            "pattern_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType);
        var constant = new BoundConstantPattern(literal);
        var discard = new BoundDiscardPattern(booleanType);

        var rewritten = PatternOptimizer.Rewrite(new BoundOrPattern(constant, discard));

        Assert.IsType<BoundOrPattern>(rewritten);
    }
}
