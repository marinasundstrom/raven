namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public partial class SyntaxNodeTest
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FindTrivia_FindsTrailingTriviaAfterTokenText(bool leading)
    {
        var token = Identifier("name").WithTrailingTrivia(Whitespace("   "));
        if (leading)
            token = token.WithLeadingTrivia(Whitespace("  "));
        var node = IdentifierName(token);
        var actualToken = node.GetFirstToken();
        var trailingStart = (leading ? 2 : 0) + 4;

        Assert.Equal(trailingStart, actualToken.TrailingTrivia[0].FullSpan.Start);
        for (var position = trailingStart; position < trailingStart + 3; position++)
        {
            Assert.Equal(SyntaxKind.WhitespaceTrivia, node.FindTrivia(position).Kind);
            Assert.Equal("   ", node.FindTrivia(position).ToString());
        }
        Assert.Equal(default, node.FindTrivia(leading ? 2 : 0));
        Assert.Equal(default, node.FindTrivia(trailingStart + 3));
        if (leading)
            Assert.Equal("  ", node.FindTrivia(0).ToString());
    }
}
