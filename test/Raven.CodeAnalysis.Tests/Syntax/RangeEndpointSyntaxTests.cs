using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax;

public class RangeEndpointSyntaxTests
{
    [Theory]
    [InlineData("let r = 1..=3")]
    [InlineData("let r = ..=3")]
    [InlineData("for x in 1..=3 { }")]
    [InlineData("let b = 3 is 1..=3")]
    public void InclusiveRange_Parses(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.DoesNotContain(tree.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Contains(tree.GetRoot().DescendantTokens(), t => t.Kind == SyntaxKind.EqualsToken &&
            t.Parent is RangeExpressionSyntax or RangePatternSyntax);
    }

    [Theory]
    [InlineData("let r = 1..<3")]
    [InlineData("let b = 3 is 1..<3")]
    [InlineData("let r = values[1..=]")]
    [InlineData("let r = 1..=")]
    [InlineData("let r = ..=")]
    [InlineData("let b = 3 is 1..=")]
    public void RemovedOrIncompleteRange_ReportsDiagnostic(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Contains(tree.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
    [Fact]
    public void InclusiveRange_NormalizationKeepsOperatorTogether()
    {
        var tree = SyntaxTree.ParseText("let r = 1..=3");
        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();
        Assert.Contains("1..=3", normalized);
        Assert.Empty(SyntaxTree.ParseText(normalized).GetDiagnostics());
    }

}
