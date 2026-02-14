using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class MatchStatementSyntaxTests
{
    [Fact]
    public void MatchStatement_PrefixForm_ParsesAsMatchStatementSyntax()
    {
        const string code = """
match true {
    0 => "zero"
    _ => "other"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        Assert.IsType<LiteralExpressionSyntax>(match.Expression);
        Assert.Equal(2, match.Arms.Count);
    }

    [Fact]
    public void MatchStatement_PrefixForm_AfterPreviousStatement_DoesNotParseAsSuffix()
    {
        const string code = """
let value: bool = true

match value {
    true => { 1 }
    false => { 0 }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        Assert.Equal(2, root.Members.Count);
        var firstGlobal = Assert.IsType<GlobalStatementSyntax>(root.Members[0]);
        var local = Assert.IsType<LocalDeclarationStatementSyntax>(firstGlobal.Statement);
        var initializer = local.Declaration.Declarators[0].Initializer;
        Assert.NotNull(initializer);
        Assert.IsType<LiteralExpressionSyntax>(initializer.Value);

        var secondGlobal = Assert.IsType<GlobalStatementSyntax>(root.Members[1]);
        var match = Assert.IsType<MatchStatementSyntax>(secondGlobal.Statement);
        Assert.IsType<IdentifierNameSyntax>(match.Expression);
        Assert.Equal(2, match.Arms.Count);
    }
}
