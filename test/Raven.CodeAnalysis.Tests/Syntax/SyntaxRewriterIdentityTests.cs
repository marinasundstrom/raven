using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SyntaxRewriterIdentityTests
{
    [Fact]
    public void NullableTokenVisitor_PreservesPresentAndAbsentValues()
    {
        var rewriter = new IdentityRewriter();
        SyntaxToken? present = SyntaxFactory.Identifier("value");
        SyntaxToken? empty = default(SyntaxToken);

        Assert.Equal(present, rewriter.VisitToken(present));
        Assert.Equal(empty, rewriter.VisitToken(empty));
        Assert.Null(rewriter.VisitToken((SyntaxToken?)null));
    }

    [Fact]
    public void NullableTokenVisitor_DispatchesPresentValuesToTokenOverride()
    {
        var rewriter = new ReplacingRewriter();
        SyntaxToken? present = SyntaxFactory.Identifier("value");
        var result = rewriter.VisitToken(present);
        Assert.True(result.HasValue);
        Assert.Equal("changed", result.Value.Text);
        Assert.Equal(1, rewriter.Calls);
        Assert.Null(rewriter.VisitToken((SyntaxToken?)null));
        Assert.Equal(1, rewriter.Calls);
    }

    [Fact]
    public void TriviaVisitor_PreservesWhitespaceAndComments()
    {
        var rewriter = new IdentityRewriter();
        var trivia = new[] { SyntaxFactory.Whitespace("  "), SyntaxFactory.Comment("// comment") };
        foreach (var item in trivia)
            Assert.Equal(item, rewriter.VisitTrivia(item));
        var list = SyntaxFactory.TriviaList(trivia);
        var rewritten = rewriter.VisitList(list);
        Assert.Equal(list.ToString(), rewritten.ToString());
        Assert.Equal(list.ToArray(), rewritten.ToArray());
    }

    private sealed class IdentityRewriter : SyntaxRewriter;

    private sealed class ReplacingRewriter : SyntaxRewriter
    {
        public int Calls { get; private set; }
        public override SyntaxToken VisitToken(SyntaxToken token)
        {
            Calls++;
            return SyntaxFactory.Identifier("changed");
        }
    }
}
