using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class CollectionComprehensionSyntaxTests
{
    [Fact]
    public void CollectionComprehension_ParsesWithoutFilter()
    {
        var tree = SyntaxTree.ParseText("val xs = [for n in numbers => n * n]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var comprehension = Assert.IsType<CollectionComprehensionElementSyntax>(collection.Elements[0]);
        Assert.Equal("for", comprehension.ForKeyword.Text);
        Assert.Equal(SyntaxKind.None, comprehension.BindingKeyword.Kind);
        Assert.Equal("n", Assert.IsType<IdentifierNameSyntax>(comprehension.Target).Identifier.Text);
        Assert.Equal("in", comprehension.InKeyword.Text);
        Assert.Equal(SyntaxKind.None, comprehension.IfKeyword.Kind);
        Assert.Null(comprehension.Condition);
    }

    [Fact]
    public void CollectionComprehension_ParsesWithFilter()
    {
        var tree = SyntaxTree.ParseText("val xs = [for n in numbers if n % 2 == 0 => n]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var comprehension = Assert.IsType<CollectionComprehensionElementSyntax>(collection.Elements[0]);
        Assert.Equal("if", comprehension.IfKeyword.Text);
        Assert.NotNull(comprehension.Condition);
        Assert.Equal("n", Assert.IsType<IdentifierNameSyntax>(comprehension.Selector).Identifier.Text);
    }

    [Fact]
    public void CollectionExpression_Spread_DotDotDot_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [...items]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var spread = Assert.IsType<SpreadElementSyntax>(collection.Elements[0]);
        Assert.Equal(SyntaxKind.DotDotDotToken, spread.DotDotToken.Kind);
    }

    [Fact]
    public void MutableCollectionExpression_ParsesLeadingExclamationToken()
    {
        var tree = SyntaxTree.ParseText("val xs = ![1; 2; 3]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        Assert.Equal(SyntaxKind.ExclamationToken, collection.ExclamationToken.Kind);
        Assert.True(collection.IsMutableByDefault);
        Assert.False(collection.IsImmutableByDefault);
    }

    [Fact]
    public void ArrayExpression_ParsesPipeDelimitedLiteral()
    {
        var tree = SyntaxTree.ParseText("val xs = [|1, 2, 3|]");
        var array = tree.GetRoot().DescendantNodes().OfType<ArrayExpressionSyntax>().Single();

        Assert.Equal(SyntaxKind.OpenArrayToken, array.OpenArrayToken.Kind);
        Assert.Equal(SyntaxKind.CloseArrayToken, array.CloseArrayToken.Kind);
        Assert.True(array.IsArrayByDefault);
        Assert.Equal(3, array.Elements.Count);
    }

    [Fact]
    public void CollectionExpression_RangeElement_ParsesAsRangeExpression()
    {
        var tree = SyntaxTree.ParseText("val xs = [1..10, 42]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var element = Assert.IsType<ExpressionElementSyntax>(collection.Elements[0]);
        var range = Assert.IsType<RangeExpressionSyntax>(element.Expression);

        Assert.Equal(SyntaxKind.DotDotToken, range.DotDotToken.Kind);
    }

    [Fact]
    public void CollectionExpression_DictionaryElement_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [\"a\": 1, \"b\": 2]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var first = Assert.IsType<DictionaryElementSyntax>(collection.Elements[0]);
        Assert.IsType<LiteralExpressionSyntax>(first.Key);
        Assert.Equal(SyntaxKind.ColonToken, first.ColonToken.Kind);
        Assert.IsType<LiteralExpressionSyntax>(first.Value);

        var second = Assert.IsType<DictionaryElementSyntax>(collection.Elements[1]);
        Assert.IsType<LiteralExpressionSyntax>(second.Key);
        Assert.Equal(SyntaxKind.ColonToken, second.ColonToken.Kind);
        Assert.IsType<LiteralExpressionSyntax>(second.Value);
    }

    [Fact]
    public void CollectionExpression_DictionarySpreadElement_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [...\"a\": 1, ...otherMap]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var first = Assert.IsType<DictionarySpreadElementSyntax>(collection.Elements[0]);
        Assert.Equal(SyntaxKind.DotDotDotToken, first.DotDotToken.Kind);
        Assert.Equal(SyntaxKind.ColonToken, first.ColonToken.Kind);

        var second = Assert.IsType<SpreadElementSyntax>(collection.Elements[1]);
        Assert.Equal(SyntaxKind.DotDotDotToken, second.DotDotToken.Kind);
    }

    [Fact]
    public void CollectionExpression_DictionaryComprehension_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [for n in numbers if n > 0 => n: n * n]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var comprehension = Assert.IsType<DictionaryComprehensionElementSyntax>(collection.Elements[0]);
        Assert.Equal("if", comprehension.IfKeyword.Text);
        Assert.NotNull(comprehension.Condition);
        Assert.IsType<IdentifierNameSyntax>(comprehension.KeySelector);
        Assert.NotNull(comprehension.ValueSelector);
    }

    [Fact]
    public void CollectionComprehension_WithDeconstructionTarget_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [for val (id, name) in people => name]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var comprehension = Assert.IsType<CollectionComprehensionElementSyntax>(collection.Elements[0]);
        Assert.Equal(SyntaxKind.ValKeyword, comprehension.BindingKeyword.Kind);
        var target = Assert.IsType<PositionalPatternSyntax>(comprehension.Target);
        Assert.Equal(2, target.Elements.Count);
    }

    [Fact]
    public void CollectionExpression_DictionaryComprehension_WithDeconstructionTarget_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [for val (key, value) in pairs => key: value]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var comprehension = Assert.IsType<DictionaryComprehensionElementSyntax>(collection.Elements[0]);
        Assert.Equal(SyntaxKind.ValKeyword, comprehension.BindingKeyword.Kind);
        var target = Assert.IsType<PositionalPatternSyntax>(comprehension.Target);
        Assert.Equal(2, target.Elements.Count);
    }

    [Fact]
    public void CollectionComprehension_WithGuardedBindingTarget_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [for val (id, amount when 100..300) in orders => amount]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var comprehension = Assert.IsType<CollectionComprehensionElementSyntax>(collection.Elements[0]);
        var target = Assert.IsType<PositionalPatternSyntax>(comprehension.Target);
        var guarded = Assert.IsType<GuardedPatternSyntax>(target.Elements[1].Pattern);

        Assert.IsType<VariablePatternSyntax>(guarded.Pattern);
        Assert.IsType<RangePatternSyntax>(guarded.WhenClause.Guard);
    }

    [Fact]
    public void CollectionExpression_CommaSeparated_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [1, 2, 3]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        Assert.Equal(3, collection.Elements.Count);
        Assert.True(collection.IsImmutableByDefault);
    }

    [Fact]
    public void CollectionExpression_SemicolonSeparated_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [1; 2; 3]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        Assert.Equal(3, collection.Elements.Count);
        Assert.True(collection.IsImmutableByDefault);
    }

    [Fact]
    public void CollectionExpression_Separatorless_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [1]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        Assert.Single(collection.Elements);
    }

    [Fact]
    public void CollectionExpression_MixedSeparators_Parses()
    {
        var tree = SyntaxTree.ParseText("val xs = [1, 2; 3]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        Assert.Equal(3, collection.Elements.Count);
    }
}
