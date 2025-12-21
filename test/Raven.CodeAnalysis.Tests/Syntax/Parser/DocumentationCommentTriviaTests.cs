namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

using Raven.CodeAnalysis.Documentation;

public class DocumentationCommentTriviaTests
{
    [Fact]
    public void SingleLineDocumentationComment_MergesContiguousLinesIntoSingleTrivia()
    {
        var code = """
/// <summary>
/// Returns a value.
/// </summary>
func Foo() {}
""";

        var documentationTrivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .Where(t => t.Kind == SyntaxKind.SingleLineDocumentationCommentTrivia)
            .ToList();

        documentationTrivia.Count.ShouldBe(1);
        documentationTrivia[0].Text.ShouldBe("/// <summary>\n/// Returns a value.\n/// </summary>\n");
    }

    [Fact]
    public void MultiLineDocumentationComment_ProducesMultilineTrivia()
    {
        var code = """
/**
 * <summary>
 * Returns a value.
 * </summary>
 */
func Foo() {}
""";

        var documentationTrivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .FirstOrDefault(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia);

        documentationTrivia.ShouldNotBeNull();

        DocumentationComment.TryParse(documentationTrivia!, DocumentationFormat.Xml, out var comment)
            .ShouldBeTrue();

        comment.ShouldNotBeNull();
        comment!.IsMultiline.ShouldBeTrue();
        comment.Content.ShouldBe("<summary>\nReturns a value.\n</summary>");
    }
}
