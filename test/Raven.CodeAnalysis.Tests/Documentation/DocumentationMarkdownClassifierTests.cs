using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class DocumentationMarkdownClassifierTests
{
    [Fact]
    public void Classify_RecognizesMarkdownConstructsAndKeepsCodeLiteral()
    {
        const string source = """
/// # Parsing
///
/// Uses [`Widget`](xref:T:Widget) and `@result`.
///
/// @result The parsed widget.
/// ```
/// @throws This is example code, not a tag.
/// ```
class Parser {}
""";

        var tree = SyntaxTree.ParseText(source);
        var trivia = tree.GetRoot()
            .DescendantTokens(descendIntoTrivia: true)
            .SelectMany(token => token.LeadingTrivia.Concat(token.TrailingTrivia))
            .Single(value => value.Kind == SyntaxKind.DocumentationCommentTrivia);

        var classifications = DocumentationMarkdownClassifier.Classify(trivia);
        var classifiedText = classifications
            .Select(classification => (
                Text: tree.GetText().ToString(classification.Span),
                classification.Kind))
            .ToArray();

        Assert.Contains(classifiedText, item => item is ("#", DocumentationMarkdownKind.Heading));
        Assert.Contains(classifiedText, item => item is ("[`Widget`](xref:T:Widget)", DocumentationMarkdownKind.Link));
        Assert.Contains(classifiedText, item => item is ("`@result`", DocumentationMarkdownKind.Code));
        Assert.Contains(classifiedText, item => item is ("@result", DocumentationMarkdownKind.Tag));
        Assert.Contains(classifiedText, item => item is ("@throws This is example code, not a tag.", DocumentationMarkdownKind.Code));
        Assert.DoesNotContain(
            classifiedText,
            item => item is ("@throws", DocumentationMarkdownKind.Tag));
    }
}

