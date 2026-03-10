using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class DirectiveTriviaTests
{
    [Fact]
    public void HashLine_IsLexedAsTrivia()
    {
        var tree = SyntaxTree.ParseText(
            """
#pragma warning disable RAV0103
func Main() {}
""");

        Assert.Empty(tree.GetDiagnostics());

        var trivia = Assert.Single(
            tree.GetRoot()
                .DescendantTrivia()
                .Where(item => item.Kind == SyntaxKind.DirectiveTrivia));

        Assert.Equal("#pragma warning disable RAV0103", trivia.Text);
    }

    [Fact]
    public void HashLine_WithArbitraryText_DoesNotProduceSyntaxDiagnostics()
    {
        var tree = SyntaxTree.ParseText(
            """
# this is treated as directive trivia
func Main() {}
""");

        Assert.Empty(tree.GetDiagnostics());
    }
}
