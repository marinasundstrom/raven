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
        Assert.Equal("n", comprehension.Identifier.Text);
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
    public void CollectionExpression_RangeElement_ParsesAsRangeExpression()
    {
        var tree = SyntaxTree.ParseText("val xs = [1..10, 42]");
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var element = Assert.IsType<ExpressionElementSyntax>(collection.Elements[0]);
        var range = Assert.IsType<RangeExpressionSyntax>(element.Expression);

        Assert.Equal(SyntaxKind.DotDotToken, range.DotDotToken.Kind);
    }
}
