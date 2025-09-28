using System.Linq;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class TuplePatternSyntaxTests
{
    [Fact]
    public void MatchArm_WithTuplePattern_Parses()
    {
        const string code = """
let value: object = (1, "two")

let result = match value {
    (int first, string second) => second
    _ => ""
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchArmSyntax>().First();

        var tuplePattern = Assert.IsType<TuplePatternSyntax>(match.Pattern);
        Assert.Equal(2, tuplePattern.Patterns.Count);
        Assert.All(tuplePattern.Patterns, pattern => Assert.IsType<DeclarationPatternSyntax>(pattern));
    }
}
