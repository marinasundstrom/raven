namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

using System.IO;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

public class DocumentationCommentTriviaTests
{
    [Fact]
    public void SingleLineDocumentationComment_MergesContiguousLinesIntoMultilineTrivia()
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
            .Where(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia)
            .ToList();

        documentationTrivia.Count.ShouldBe(1);
        documentationTrivia[0].Text.ShouldBe("/// <summary>\n/// Returns a value.\n/// </summary>\n");
    }

    [Fact]
    public void SingleLineDocumentationComment_WithoutAdditionalLines_RemainsSingleLineTrivia()
    {
        var code = """
/// Hello
func Foo() {}
""";

        var documentationTrivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .Where(t => t.Kind == SyntaxKind.SingleLineDocumentationCommentTrivia)
            .ToList();

        documentationTrivia.Count.ShouldBe(1);
        documentationTrivia[0].Text.ShouldBe("/// Hello\n");
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

    [Fact]
    public void SingleLineDocComments_DoNotEatNewlineTokens_WhenNewlinesAreSignificant()
    {
        var code = """
Foo()

/// <summary>
/// Returns a value.
/// </summary>
func Foo() {}
""";

        var lexer = new Lexer(new StringReader(code));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new StatementSyntaxParser(context);

        var firstStatement = parser.ParseStatement().CreateRed();
        var secondStatement = parser.ParseStatement().CreateRed();

        firstStatement.GetLastToken().Kind.ShouldBe(SyntaxKind.NewLineToken);

        var nextFirstToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        nextFirstToken.LeadingTrivia.ShouldContain(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia);
    }
}
