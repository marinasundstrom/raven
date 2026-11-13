using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Documentation;

[Trait("Target", "Shared")]
public sealed class DocumentationCommentUtilitiesTests
{
    [Fact]
    public void NullAndUndocumentedNodesReturnEmptyOutputs()
    {
        foreach (var node in new SyntaxNode?[] { null, Declaration("class C {}") })
        {
            Assert.False(DocumentationCommentUtilities.TryGetDocumentationComment(node, DocumentationFormat.Markdown,
                out var comment, out var trivia));
            Assert.Null(comment);
            Assert.Empty(trivia);
        }
    }

    [Theory]
    [InlineData("/// Earlier\n\n/// Later\n", true)]
    [InlineData("/// Earlier\n// boundary\n/// Later\n", false)]
    public void CollectsNearestCommentsInSourceOrder(string prefix, bool includesEarlier)
    {
        var node = Declaration(prefix + "class C {}");
        Assert.True(DocumentationCommentUtilities.TryGetDocumentationComment(node, DocumentationFormat.Markdown,
            out var comment, out var trivia));
        Assert.Contains("Later", comment!.Body);
        Assert.Equal(includesEarlier, comment.Body.Contains("Earlier"));
        Assert.Equal(includesEarlier ? 2 : 1, trivia.Count);
        Assert.All(trivia, item => Assert.Equal(SyntaxKind.DocumentationCommentTrivia, item.Kind));
        Assert.True(DocumentationCommentUtilities.TryGetDocumentationComment(node, DocumentationFormat.Markdown, out var simple));
        Assert.Equal(comment, simple);
    }

    [Fact]
    public void OrdinaryCommentNextToDeclarationStopsCollection()
    {
        Assert.False(DocumentationCommentUtilities.TryGetDocumentationComment(
            Declaration("/// Earlier\n// boundary\nclass C {}"), DocumentationFormat.Markdown, out var comment));
        Assert.Null(comment);
    }

    [Fact]
    public void EmptyDeclarationsHaveNoMergedComment()
    {
        Assert.Empty(DocumentationCommentUtilities.GetDocumentationComments([], new ParseOptions()));
        Assert.Null(DocumentationCommentUtilities.GetMergedDocumentationComment([], new ParseOptions()));
    }

    [Fact]
    public void DeclarationOrderAndTreeFormatOverrideFallbackOptions()
    {
        var first = Declaration("/// First\nclass A {}", DocumentationFormat.Markdown).GetReference();
        var undocumented = Declaration("class B {}").GetReference();
        var second = Declaration("/// Second\nclass C {}", DocumentationFormat.Xml).GetReference();
        var fallback = new ParseOptions { DocumentationFormat = DocumentationFormat.Xml };
        var comments = DocumentationCommentUtilities.GetDocumentationComments([first, undocumented, second], fallback);
        Assert.Equal(2, comments.Length);
        Assert.Equal(DocumentationFormat.Markdown, comments[0].Format);
        Assert.Equal(DocumentationFormat.Xml, comments[1].Format);
        var merged = DocumentationCommentUtilities.GetMergedDocumentationComment([first, undocumented, second], fallback);
        Assert.Equal(DocumentationFormat.Markdown, merged!.Format);
        Assert.Equal(comments[0].Body + Environment.NewLine + comments[1].Body, merged.Body);
        Assert.Equal(comments[0], DocumentationCommentUtilities.GetMergedDocumentationComment([first], fallback));
    }

    private static SyntaxNode Declaration(string source, DocumentationFormat format = DocumentationFormat.Markdown)
        => SyntaxTree.ParseText(source, new ParseOptions { DocumentationMode = true, DocumentationFormat = format })
            .GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
}
