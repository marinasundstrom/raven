using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class PatternHeaderBodyBoundaryTests
{
    [Theory]
    [InlineData("value is Widget", "result")]
    [InlineData("value is Widget", "owner.Value")]
    [InlineData("ready && value is Widget", "result")]
    [InlineData("value is Widget", "")]
    public void ConditionalExpressionRetainsBodyAfterTypeTest(string condition, string body)
    {
        var source = $"let answer = if {condition} {{ {body} }} else {{ fallback }}";
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<IfExpressionSyntax>());
        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<PropertyPatternSyntax>());
        Assert.Equal(2, tree.GetRoot().DescendantNodes().OfType<BlockSyntax>().Count());
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Theory]
    [InlineData("if")]
    [InlineData("while")]
    public void StatementHeaderRetainsSingleValueBody(string keyword)
    {
        var source = $"{keyword} value is Widget {{ result }}";
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<PropertyPatternSyntax>());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>());
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void ExplicitPropertyPatternStillBelongsToHeader()
    {
        const string source = "let answer = if value is Widget { Name: not null } { result } else { fallback }";
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<PropertyPatternSyntax>());
        Assert.Equal(2, tree.GetRoot().DescendantNodes().OfType<BlockSyntax>().Count());
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }
}
