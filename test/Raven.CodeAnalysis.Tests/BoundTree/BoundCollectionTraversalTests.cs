using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundCollectionTraversalTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DictionaryTraversalPreservesEntrySpreadAndComprehensionOrder(bool hasCondition)
    {
        var type = Named("A", "Example.Value");
        var values = Enumerable.Range(0, 7).Select(value =>
            new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type)).ToArray();
        var local = Make<LocalSymbol>(SymbolKind.Local, "item");
        var entry = new DictionaryEntryBinding(key: values[0], value: values[1]);
        var spread = new DictionarySpreadBinding(expression: values[2]);
        var comprehension = new DictionaryComprehensionBinding(values[3], local,
            hasCondition ? values[4] : null, values[5], values[6], type, type);
        var expression = new BoundDictionaryExpression(type, [entry, spread, comprehension], local,
            BoundExpressionReason.OtherError);
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal<BoundLiteralExpression>(hasCondition ? values : values.Where((_, index) => index != 4), walker.Literals);
        Assert.Same(local, expression.CollectionSymbol);
        Assert.Same(type, expression.Symbol);
        Assert.Equal(BoundExpressionReason.OtherError, expression.Reason);
        Assert.Same(expression, expression.Update(type, expression.Elements, local, expression.Reason));
        Assert.NotEqual(entry, new DictionaryEntryBinding(values[0], values[1]));
        Assert.Same(local, comprehension.IterationLocal);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CollectionTraversalIncludesSpreadAndOptionalComprehensionCondition(bool hasCondition)
    {
        var type = Named("A", "Example.Value");
        var values = Enumerable.Range(0, 4).Select(value =>
            new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type)).ToArray();
        var local = Make<LocalSymbol>(SymbolKind.Local, "item");
        var comprehension = new BoundCollectionComprehensionExpression(type, values[1], local,
            hasCondition ? values[2] : null, values[3], type);
        var expression = new BoundCollectionExpression(type, [new BoundSpreadElement(values[0]), comprehension]);
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal<BoundLiteralExpression>(hasCondition ? values : values.Where((_, index) => index != 2), walker.Literals);
        Assert.Same(type, comprehension.ElementType);
        Assert.Same(local, comprehension.IterationLocal);
    }

    [Fact]
    public void EmptyAndSpreadExpressionsPreserveAbsentTypes()
    {
        var empty = new BoundEmptyCollectionExpression(reason: BoundExpressionReason.MissingType);
        var spread = new BoundSpreadElement(empty);
        Assert.Null(empty.Type);
        Assert.Null(spread.Type);
        Assert.Null(spread.Symbol);
        Assert.Equal(BoundExpressionReason.MissingType, spread.Reason);
        Assert.Same(empty, spread.Expression);
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Literals { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals.Add(node);
    }
}

[Trait("Target", "DotNet")]
public sealed class BoundDictionaryHierarchyTests
{
    [Fact]
    public void DictionaryBindingsDeclareTheirClosedFamily()
    {
        var root = typeof(DictionaryElementBinding);
        var closure = Assert.Single(root.GetCustomAttributesData(), attribute =>
            attribute.AttributeType.FullName == "System.Runtime.CompilerServices.IsClosedTypeAttribute");
        var argument = Assert.Single(closure.NamedArguments, value => value.MemberName == "DerivedTypes");
        var children = Assert.IsAssignableFrom<IEnumerable<System.Reflection.CustomAttributeTypedArgument>>(argument.TypedValue.Value)
            .Select(value => Assert.IsAssignableFrom<Type>(value.Value)).OrderBy(type => type.Name).ToArray();
        Type[] expected = [typeof(DictionaryEntryBinding), typeof(DictionarySpreadBinding), typeof(DictionaryComprehensionBinding)];
        Assert.Equal(expected.OrderBy(type => type.Name), children);
        Assert.All(children, type => Assert.True(type.IsSealed));
        Assert.True(root.IsAbstract);
    }
}
