#if RAVEN_DOCUMENTATION_PORT
extern alias Port;
using DocumentationComment = Port::Raven.CodeAnalysis.Documentation.DocumentationComment;
using DocumentationTag = Port::Raven.CodeAnalysis.Documentation.DocumentationTag;
using DocumentationTagKind = Port::Raven.CodeAnalysis.Documentation.DocumentationTagKind;
#endif

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Documentation;

[Trait("Target", "Shared")]
public sealed class DocumentationCommentValueTests
{
    [Fact]
    public void CommentsHaveStructuralValueEqualityIncludingTags()
    {
        const string content = "Body\n@param value Input.\n@returns Output.";
        var first = DocumentationComment.Create(DocumentationFormat.Markdown, content);
        var second = DocumentationComment.Create(DocumentationFormat.Markdown, content);
        Assert.NotSame(first, second);
        Assert.NotSame(first.Tags[0], second.Tags[0]);
        Assert.True(first.Equals(second));
        Assert.True(first.Equals((object)second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.True(new HashSet<DocumentationComment> { first }.Contains(second));
        Assert.False(first.Equals((DocumentationComment?)null));
        Assert.False(first.Equals(new object()));
        Assert.True(first != null);
        Assert.False(null == first);
        Assert.True((DocumentationComment?)null == null);
        Assert.NotEqual(first, DocumentationComment.Create(DocumentationFormat.Xml, content));
        Assert.NotEqual(first, DocumentationComment.Create(DocumentationFormat.Markdown, content, "different raw"));
        Assert.NotEqual(first, DocumentationComment.Create(DocumentationFormat.Markdown, content + "more"));
    }

    [Fact]
    public void TagsRetainRecordClassValueSemantics()
    {
        var first = new DocumentationTag(DocumentationTagKind.Remarks, "remarks", null, "Details");
        var second = new DocumentationTag(DocumentationTagKind.Remarks, "remarks", null, "Details");
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, first with { Content = "Other" });
        Assert.False(first.Equals((DocumentationTag?)null));
        var (kind, name, argument, content) = first;
        Assert.Equal(DocumentationTagKind.Remarks, kind);
        Assert.Equal("remarks", name);
        Assert.Null(argument);
        Assert.Equal("Details", content);
        Assert.True(typeof(DocumentationTag).IsSealed);
        Assert.True(typeof(DocumentationComment).IsSealed);
    }

    [Theory]
    [InlineData("param", DocumentationTagKind.Param)]
    [InlineData("parameter", DocumentationTagKind.Param)]
    [InlineData("typeparam", DocumentationTagKind.TypeParam)]
    [InlineData("typeparameter", DocumentationTagKind.TypeParam)]
    [InlineData("return", DocumentationTagKind.Returns)]
    [InlineData("returns", DocumentationTagKind.Returns)]
    [InlineData("result", DocumentationTagKind.Returns)]
    [InlineData("value", DocumentationTagKind.Value)]
    [InlineData("remarks", DocumentationTagKind.Remarks)]
    [InlineData("notes", DocumentationTagKind.Remarks)]
    [InlineData("example", DocumentationTagKind.Example)]
    [InlineData("examples", DocumentationTagKind.Example)]
    [InlineData("exception", DocumentationTagKind.Exception)]
    [InlineData("throws", DocumentationTagKind.Exception)]
    [InlineData("see", DocumentationTagKind.See)]
    [InlineData("seealso", DocumentationTagKind.SeeAlso)]
    [InlineData("inheritdoc", DocumentationTagKind.InheritDoc)]
    [InlineData("inherit", DocumentationTagKind.InheritDoc)]
    [InlineData("custom", DocumentationTagKind.Unknown)]
    public void MarkdownTagsPreserveKindsNamesAndArguments(string name, DocumentationTagKind kind)
    {
        var comment = DocumentationComment.Create(DocumentationFormat.Markdown, "Body\n@" + name + " target Description\ncontinued");
        Assert.Equal("Body", comment.Body);
        var tag = Assert.Single(comment.Tags);
        Assert.Equal(kind, tag.Kind);
        Assert.Equal(name, tag.Name);
        var hasArgument = kind is DocumentationTagKind.Param or DocumentationTagKind.TypeParam or DocumentationTagKind.Exception or DocumentationTagKind.See or DocumentationTagKind.SeeAlso;
        Assert.Equal(hasArgument ? "target" : null, tag.Argument);
        Assert.Equal((hasArgument ? "Description" : "target Description") + "\ncontinued", tag.Content);
    }

    [Fact]
    public void TriviaNormalizationPreservesPartialTabIndentationAndRawText()
    {
        const string raw = "///  One\r\n/// \tTwo\r\n";
        Assert.True(DocumentationComment.TryParse(SyntaxFactory.Trivia(SyntaxKind.DocumentationCommentTrivia, raw), DocumentationFormat.Markdown, out var comment));
        Assert.Equal(raw, comment!.RawText);
        Assert.Equal("One" + Environment.NewLine + "   Two", comment.Content);
        Assert.True(comment.IsMultiline);
        Assert.False(DocumentationComment.TryParse(SyntaxFactory.Space, DocumentationFormat.Markdown, out var absent));
        Assert.Null(absent);
    }

    [Fact]
    public void BlankLineTrimmingPreservesInternalLinesAndTagContinuation()
    {
        var content = new string('\n', 2000) + "Body\n\nMore\n\n@param x First\n\nSecond" + new string('\n', 2000);
        var comment = DocumentationComment.Create(DocumentationFormat.Markdown, content);
        Assert.Equal("Body\n\nMore", comment.Body);
        Assert.Equal("First\n\nSecond", Assert.Single(comment.Tags).Content);
        var empty = DocumentationComment.Create(DocumentationFormat.Markdown, "@returns\n\n");
        Assert.Equal(string.Empty, Assert.Single(empty.Tags).Content);
    }

    [Fact]
    public void XmlAndWhitespaceContentArePreservedWithoutMarkdownTags()
    {
        const string xml = "<summary>Summary</summary>\n@param text";
        var comment = DocumentationComment.Create(DocumentationFormat.Xml, xml);
        Assert.Equal(xml, comment.Content);
        Assert.Equal(xml, comment.Body);
        Assert.Empty(comment.Tags);
        Assert.True(comment.IsMultiline);
        var whitespace = DocumentationComment.Create(DocumentationFormat.Markdown, " \t");
        Assert.Equal(" \t", whitespace.Body);
        Assert.False(whitespace.IsMultiline);
    }

    [Fact]
    public void MergePreservesOrderAndSingleCommentIdentity()
    {
        var first = DocumentationComment.Create(DocumentationFormat.Markdown, "First\n@param x Value");
        var second = DocumentationComment.Create(DocumentationFormat.Markdown, "Second\n@returns Result");
        Assert.Same(first, DocumentationComment.Merge(DocumentationFormat.Markdown, [first]));
        Assert.Throws<ArgumentException>(() => DocumentationComment.Merge(DocumentationFormat.Markdown, []));
        var merged = DocumentationComment.Merge(DocumentationFormat.Markdown, [first, second]);
        Assert.Equal("First" + Environment.NewLine + "Second", merged.Body);
        Assert.Equal(first.RawText + Environment.NewLine + second.RawText, merged.RawText);
        Assert.Equal(first.Content + Environment.NewLine + second.Content, merged.Content);
        Assert.Equal(new[] { DocumentationTagKind.Param, DocumentationTagKind.Returns }, merged.Tags.Select(tag => tag.Kind));
        Assert.True(merged.IsMultiline);
    }
}
