using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class PatternOptimizerContractTests
{
    [Fact]
    public void ConstantPatternPreservesLiteralAndDesignatorContracts()
    {
        var type = Named("A", "Example.Value");
        var local = Make<LocalSymbol>(SymbolKind.Local, "value");
        Set(local, "get_Type", type);
        var designator = new BoundSingleVariableDesignator(local);
        var literalType = new LiteralTypeSymbol(type, 42, Make<AssemblySymbol>(SymbolKind.Assembly, "A"));
        var pattern = new BoundConstantPattern(literalType, designator);
        Assert.Same(literalType, pattern.Type);
        Assert.Same(literalType, pattern.LiteralType);
        Assert.Null(pattern.Expression);
        Assert.Equal(42, pattern.ConstantValue);
        Assert.Same(designator, Assert.Single(pattern.GetDesignators()));
        Assert.Empty(new BoundConstantPattern(literalType).GetDesignators());
        Assert.Empty(new BoundConstantPattern(literalType, new BoundDiscardDesignator(type)).GetDesignators());
        Assert.Null(new BoundConstantPattern(new LiteralTypeSymbol(type, null, literalType.ContainingAssembly!)).ConstantValue);
    }

    [Fact]
    public void ConstantPatternReadsExpressionFieldAndMemberConstants()
    {
        var type = Named("A", "Example.Value");
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, true, type);
        var converted = new BoundConversionExpression(literal, type, new Conversion(isIdentity: true));
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", type);
        Set(field, "GetConstantValue", 42);
        var property = Make<PropertySymbol>(SymbolKind.Property, "Property");
        Set(property, "get_Type", type);
        Assert.Equal(true, new BoundConstantPattern(converted).ConstantValue);
        Assert.Equal(42, new BoundConstantPattern(new BoundFieldAccess(field)).ConstantValue);
        Assert.Equal(42, new BoundConstantPattern(new BoundMemberAccessExpression(null, field)).ConstantValue);
        Assert.Null(new BoundConstantPattern(new BoundMemberAccessExpression(null, property)).ConstantValue);
        var unknown = new BoundDefaultValueExpression(type);
        var pattern = new BoundConstantPattern(unknown);
        Assert.Same(unknown, pattern.Expression);
        Assert.Null(pattern.LiteralType);
        Assert.Null(pattern.ConstantValue);
        Assert.Throws<ArgumentNullException>(() => new BoundConstantPattern(
            new BoundNamespaceExpression(Make<NamespaceSymbol>(SymbolKind.Namespace, "Example"))));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void DiscardConjunctionPreservesDeclarationAndLocal(bool discardFirst)
    {
        var type = Named("A", "Example.Value");
        var local = Make<LocalSymbol>(SymbolKind.Local, "value");
        Set(local, "get_Type", type);
        var designator = new BoundSingleVariableDesignator(local);
        var declaration = new BoundDeclarationPattern(type, designator);
        var discard = new BoundDiscardPattern(type);
        var input = discardFirst ? new BoundAndPattern(discard, declaration) : new BoundAndPattern(declaration, discard);
        var result = Assert.IsType<BoundDeclarationPattern>(PatternOptimizer.Rewrite(input));
        Assert.Same(declaration, result);
        Assert.Same(designator, Assert.Single(result.GetDesignators()));
        Assert.Same(local, ((BoundSingleVariableDesignator)result.Designator).Local);
    }

    [Fact]
    public void LeftDiscardDisjunctionEliminatesUnreachableRightPattern()
    {
        var type = Named("A", "Example.Value");
        var discard = new BoundDiscardPattern(type);
        var guarded = new BoundGuardedPattern(discard, null, new BoundDefaultValueExpression(type));
        Assert.Same(discard, PatternOptimizer.Rewrite(new BoundOrPattern(discard, guarded)));
    }

    [Fact]
    public void RightDiscardDisjunctionRetainsLeftGuardAndEvaluationOrder()
    {
        var type = Named("A", "Example.Value");
        var discard = new BoundDiscardPattern(type);
        var guard = new BoundDefaultValueExpression(type);
        var guarded = new BoundGuardedPattern(discard, null, guard);
        var input = new BoundOrPattern(guarded, discard);
        var result = Assert.IsType<BoundOrPattern>(PatternOptimizer.Rewrite(input));
        Assert.Same(input, result);
        Assert.Same(guarded, result.Left);
        Assert.Same(guard, ((BoundGuardedPattern)result.Left).GuardExpression);
        Assert.Same(discard, result.Right);
    }

    [Fact]
    public void DoubleNegationPreservesInnerPatternAndSingleNegationRemains()
    {
        var type = Named("A", "Example.Value");
        var inner = new BoundGuardedPattern(new BoundDiscardPattern(type), null, new BoundDefaultValueExpression(type));
        var single = new BoundNotPattern(inner);
        Assert.Same(single, PatternOptimizer.Rewrite(single));
        Assert.Same(inner, PatternOptimizer.Rewrite(new BoundNotPattern(single)));
    }

    [Fact]
    public void NestedAlgebraIsSimplifiedBeforeItsParent()
    {
        var type = Named("A", "Example.Value");
        var discard = new BoundDiscardPattern(type);
        var value = new BoundConstantPattern(new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, true, type));
        var input = new BoundAndPattern(new BoundNotPattern(new BoundNotPattern(value)), new BoundOrPattern(discard, value));
        Assert.Same(value, PatternOptimizer.Rewrite(input));
    }

    [Fact]
    public void NontrivialConjunctionRetainsBothOperands()
    {
        var type = Named("A", "Example.Value");
        var first = new BoundConstantPattern(new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, true, type));
        var second = new BoundConstantPattern(new BoundLiteralExpression(BoundLiteralExpressionKind.FalseLiteral, false, type));
        var input = new BoundAndPattern(first, second);
        Assert.Same(input, PatternOptimizer.Rewrite(input));
        Assert.Same(first, input.Left);
        Assert.Same(second, input.Right);
    }
}
