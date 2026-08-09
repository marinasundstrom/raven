namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SubmissionCompletenessTests
{
    [Theory]
    [InlineData("")]
    [InlineData("   // comment")]
    [InlineData("/* comment */")]
    [InlineData("\"text\"")]
    [InlineData("\"\"\"\"\"\"")]
    [InlineData("1 + 2")]
    [InlineData("unknownName")]
    [InlineData("let value = 42")]
    [InlineData("func add(left: int, right: int) -> int { left + right }")]
    public void GetSubmissionCompleteness_CompleteSubmission_ReturnsComplete(string source)
    {
        var tree = ParseSubmission(source);

        Assert.Equal(SubmissionCompleteness.Complete, tree.GetSubmissionCompleteness());
    }

    [Theory]
    [InlineData("1 +")]
    [InlineData("1 +   ")]
    [InlineData("invoke(")]
    [InlineData("if true {")]
    [InlineData("let value =")]
    [InlineData("\"unterminated")]
    [InlineData("'x")]
    [InlineData("/* unterminated")]
    [InlineData("\"\"\"\nunterminated")]
    [InlineData("#if FEATURE\nlet value = 1")]
    [InlineData(")\nif true {")]
    public void GetSubmissionCompleteness_TrailingConstruct_ReturnsIncomplete(string source)
    {
        var tree = ParseSubmission(source);

        Assert.Equal(SubmissionCompleteness.Incomplete, tree.GetSubmissionCompleteness());
    }

    [Theory]
    [InlineData("1 2")]
    [InlineData("invoke(1, )")]
    [InlineData(")")]
    [InlineData("\"unterminated\nlet value = 1")]
    [InlineData("#else")]
    public void GetSubmissionCompleteness_CompleteSyntaxError_ReturnsInvalid(string source)
    {
        var tree = ParseSubmission(source);

        Assert.Equal(SubmissionCompleteness.Invalid, tree.GetSubmissionCompleteness());
    }

    [Fact]
    public void GetSubmissionCompleteness_InteractiveTree_IsSupported()
    {
        var tree = SyntaxTree.ParseText(
            "let value = 42",
            new ParseOptions { Kind = SourceCodeKind.Interactive });

        Assert.Equal(SubmissionCompleteness.Complete, tree.GetSubmissionCompleteness());
    }

    [Fact]
    public void GetSubmissionCompleteness_RegularTree_Throws()
    {
        var tree = SyntaxTree.ParseText("let value = 42");

        var exception = Assert.Throws<InvalidOperationException>(() => tree.GetSubmissionCompleteness());
        Assert.Contains("script or interactive", exception.Message, StringComparison.Ordinal);
    }

    [Fact]
    public void GetSubmissionCompleteness_CanceledToken_Throws()
    {
        var tree = ParseSubmission("let value = 42");
        using var source = new CancellationTokenSource();
        source.Cancel();

        Assert.Throws<OperationCanceledException>(() => tree.GetSubmissionCompleteness(source.Token));
    }

    private static SyntaxTree ParseSubmission(string source)
        => SyntaxTree.ParseText(source, new ParseOptions { Kind = SourceCodeKind.Script });
}
