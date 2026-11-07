using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundConversionContractTests
{
    [Fact]
    public void WrappersPreserveMetadataAndImmutableUpdates()
    {
        var source = Named("A", "Example.Source");
        var target = Named("A", "Example.Target");
        var local = Make<LocalSymbol>(SymbolKind.Local, "value");
        Set(local, "get_Type", source);
        var operand = new BoundLocalAccess(local, BoundExpressionReason.Inaccessible);
        var parenthesized = new BoundParenthesizedExpression(operand);
        Assert.Same(source, parenthesized.Type);
        Assert.Same(local, parenthesized.Symbol);
        Assert.Equal(operand.Reason, parenthesized.Reason);
        Assert.Same(parenthesized, parenthesized.Update(operand));

        var method = Make<MethodSymbol>(SymbolKind.Method, "op_Explicit");
        var conversion = new Conversion(isUserDefined: true, methodSymbol: method);
        var converted = new BoundConversionExpression(operand, target, conversion, true, true);
        Assert.Same(target, converted.Type);
        Assert.Same(operand, converted.Expression);
        Assert.Same(method, converted.MethodSymbol);
        Assert.True(converted.IsExplicit);
        Assert.True(converted.IsUserDefined);
        Assert.True(converted.IsChecked);
        Assert.True(converted.IsNullableSuppression);
        Assert.False(converted.IsIdentity);
        Assert.False(converted.IsBoxing);
        Assert.False(converted.IsReference);
        Assert.Same(converted, converted.Update(operand, target, conversion, true, true));
        var uncheckedCopy = converted.Update(operand, target, conversion, true, false);
        Assert.NotSame(converted, uncheckedCopy);
        Assert.False(uncheckedCopy.IsChecked);
        Assert.True(converted.IsChecked);

        var reference = new Conversion(isImplicit: true, isReference: true);
        var safeCast = new BoundAsExpression(operand, target, reference);
        Assert.Equal(reference, safeCast.Conversion);
        Assert.Same(target, safeCast.Type);
        Assert.Same(safeCast, safeCast.Update(operand, target, reference));
        var replacement = new BoundDefaultValueExpression(source);
        Assert.Same(replacement, parenthesized.Update(replacement).Expression);
        Assert.Same(replacement, safeCast.Update(replacement, target, reference).Expression);
        var visitor = new ReturningVisitor();
        Assert.Equal("parenthesized", visitor.Visit(parenthesized));
        Assert.Equal("conversion", visitor.Visit(converted));
        Assert.Equal("as", visitor.Visit(safeCast));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void BooleanConstantsUnwrapOnlyIdentityConversions(bool value)
    {
        var type = Named("A", "System.Boolean");
        var literal = new BoundLiteralExpression(value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral, value, type);
        var identity = new Conversion(isImplicit: true, isIdentity: true);
        var wrapped = new BoundParenthesizedExpression(new BoundConversionExpression(new BoundAsExpression(literal, type, identity), type, identity));
        Assert.True(BoundBooleanFacts.TryGetConstantValue(wrapped, out var actual));
        Assert.Equal(value, actual);
        foreach (var conversion in new[] { Conversion.None, new Conversion(isReference: true), new Conversion(isUserDefined: true), new Conversion(isNumeric: true) })
        {
            Assert.False(BoundBooleanFacts.TryGetConstantValue(new BoundConversionExpression(literal, type, conversion), out actual));
            Assert.False(actual);
            Assert.False(BoundBooleanFacts.TryGetConstantValue(new BoundAsExpression(literal, type, conversion), out actual));
            Assert.False(actual);
        }
        Assert.False(BoundBooleanFacts.TryGetConstantValue(new BoundDefaultValueExpression(type), out actual));
        Assert.False(actual);
        Assert.False(BoundBooleanFacts.TryGetConstantValue(new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type), out actual));
        Assert.False(actual);
        Assert.False(BoundBooleanFacts.TryGetConstantValue(new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, 1, type), out actual));
        Assert.False(actual);
        var walker = new RecordingWalker();
        walker.Visit(wrapped);
        Assert.Same(literal, Assert.Single(walker.Literals));
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Literals { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals.Add(node);
    }

    private sealed class ReturningVisitor : BoundTreeVisitor<string>
    {
        public override string VisitParenthesizedExpression(BoundParenthesizedExpression node) => "parenthesized";
        public override string VisitConversionExpression(BoundConversionExpression node) => "conversion";
        public override string VisitAsExpression(BoundAsExpression node) => "as";
    }
}
