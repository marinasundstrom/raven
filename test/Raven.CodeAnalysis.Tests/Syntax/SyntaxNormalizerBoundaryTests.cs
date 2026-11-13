using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax;

[Trait("Target", "Shared")]
public sealed class SyntaxNormalizerBoundaryTests
{
    [Theory]
    [InlineData("if value is { Item: { } captured } && Check(captured) { Work() }")]
    [InlineData("let text = \"value: ${value}\"\nPrint(text)")]
    [InlineData("Call(out _)")]
    public void InlineSyntaxKeepsTokenBoundaries(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        var normalized = new SyntaxNormalizer().Visit(tree.GetRoot()).ToFullString();
        Assert.Empty(SyntaxTree.ParseText(normalized).GetDiagnostics());
        if (source.StartsWith("if ", StringComparison.Ordinal))
            Assert.Contains("{ Item: { } captured }", normalized);
        Assert.Equal(tree.GetRoot().DescendantTokens().Select(token => token.Text),
            SyntaxTree.ParseText(normalized).GetRoot().DescendantTokens().Select(token => token.Text));
    }

    [Fact]
    public void NestedExpressionsKeepTheirTokensTogetherAndSeparateMembers()
    {
        var tree = SyntaxTree.ParseText("class C { func A() -> int { return (1 + 2) } func B() -> int { return 3 } }");
        var normalized = new SyntaxNormalizer().Visit(tree.GetRoot()).ToFullString();
        Assert.Contains("return (1 + 2)", normalized);
        Assert.Contains("return 3", normalized);
        Assert.Contains("\n    func B()", normalized);
        Assert.Empty(SyntaxTree.ParseText(normalized).GetDiagnostics());
    }

    [Fact]
    public void LargeTypeRetainsEveryMemberAndIsStableOnRepeatedNormalization()
    {
        var source = "class C {\n" + string.Join("\n", Enumerable.Range(0, 100).Select(i => $"func M{i}() -> int {{ return ({i} + 1) }}")) + "\n}";
        var tree = SyntaxTree.ParseText(source);
        var normalizer = new SyntaxNormalizer();
        var normalized = normalizer.Visit(tree.GetRoot()).ToFullString();
        Assert.Equal(100, normalized.Split("func M").Length - 1);
        var reparsed = SyntaxTree.ParseText(normalized);
        Assert.Empty(reparsed.GetDiagnostics());
        Assert.Equal(normalized, normalizer.Visit(reparsed.GetRoot()).ToFullString());
    }
}
