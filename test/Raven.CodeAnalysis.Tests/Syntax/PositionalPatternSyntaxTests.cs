using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PositionalPatternSyntaxTests
{
    [Fact]
    public void MatchArm_WithPositionalPattern_Parses()
    {
        const string code = """
let value: object = (1, "two")

let result = value match {
    (first: int, second: string) => second
    _ => ""
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchArmSyntax>().First();

        var tuplePattern = Assert.IsType<PositionalPatternSyntax>(match.Pattern);
        Assert.Equal(2, tuplePattern.Elements.Count);

        Assert.Collection(tuplePattern.Elements,
            element =>
            {
                Assert.Equal("first", element.NameColon?.Name.Identifier.ValueText);
                Assert.IsType<DeclarationPatternSyntax>(element.Pattern);
            },
            element =>
            {
                Assert.Equal("second", element.NameColon?.Name.Identifier.ValueText);
                Assert.IsType<DeclarationPatternSyntax>(element.Pattern);
            });
    }
}
