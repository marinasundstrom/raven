using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class GeneratedPatternUpdateTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void PropertyPatternClauseRetainsChangedBraceTrivia(bool opening)
    {
        var original = Clause();
        var token = (opening ? original.OpenBraceToken : original.CloseBraceToken)
            .WithTrailingTrivia(SyntaxFactory.Whitespace(" "));
        var updated = opening ? original.WithOpenBraceToken(token) : original.WithCloseBraceToken(token);
        Assert.NotSame(original, updated);
        Assert.Equal(token.ToFullString(), (opening ? updated.OpenBraceToken : updated.CloseBraceToken).ToFullString());
        Assert.Equal(original.Properties.Count, updated.Properties.Count);
        Assert.Equal(opening ? "{" : "}", (opening ? original.OpenBraceToken : original.CloseBraceToken).ToFullString());
    }

    [Fact]
    public void PropertyPatternClauseRetainsChangedPropertyList()
    {
        var original = Clause();
        var replacement = SyntaxTree.ParseText("func F({Other: let y}: object) { }")
            .GetRoot().DescendantNodes().OfType<PropertyPatternClauseSyntax>().Single().Properties;
        var updated = original.WithProperties(replacement);
        Assert.NotSame(original, updated);
        Assert.Equal(replacement[0].ToFullString(), Assert.Single(updated.Properties).ToFullString());
        Assert.Single(original.Properties);
    }

    [Fact]
    public void UnchangedPropertyPatternClauseRetainsIdentity()
    {
        var original = Clause();
        Assert.Same(original, original.Update(original.OpenBraceToken, original.Properties, original.CloseBraceToken));
    }

    private static PropertyPatternClauseSyntax Clause()
        => SyntaxTree.ParseText("func F({Value: let x}: object) { }")
            .GetRoot().DescendantNodes().OfType<PropertyPatternClauseSyntax>().Single();
}
