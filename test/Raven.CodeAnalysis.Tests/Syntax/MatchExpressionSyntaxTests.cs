using System.Linq;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class MatchExpressionSyntaxTests
{
    [Fact]
    public void MatchExpression_WithStringArmOnNewLine_ParsesArms()
    {
        const string code = """
let value = "hi"

let result = value match {
    "hi" => "match"
    _ => "default"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);

        Assert.Collection(
            match.Arms,
            arm =>
            {
                Assert.Equal("\"hi\"", arm.Pattern.ToString());
                Assert.False(arm.Expression.IsMissing);
                Assert.Equal("\"match\"", arm.Expression.ToString());
            },
            arm =>
            {
                Assert.Equal("_", arm.Pattern.ToString());
                Assert.False(arm.Expression.IsMissing);
                Assert.Equal("\"default\"", arm.Expression.ToString());
            });
    }
}
