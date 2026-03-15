using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class DocumentationDisplayFormatterTests
{
    [Fact]
    public void XmlDocumentation_IsRenderedAsMarkdownSections()
    {
        var comment = DocumentationComment.Create(
            DocumentationFormat.Xml,
            """
<summary>
Creates a stored property declaration with an initializer.
</summary>
<remarks>
Alias for <c>PropertyDeclaration</c>. Prefer the canonical factory name unless the alias is clearer at the call site.
</remarks>
""");

        var formatted = DocumentationDisplayFormatter.FormatForMarkdown(comment);

        formatted.ShouldNotBeNull();
        formatted.ShouldContain("Creates a stored property declaration with an initializer.");
        formatted.ShouldContain("**Remarks**");
        formatted.ShouldContain("`PropertyDeclaration`");
        formatted.ShouldNotContain("<summary>");
        formatted.ShouldNotContain("<remarks>");
    }

    [Fact]
    public void MarkdownDocumentationTags_AreRenderedAsStructuredSections()
    {
        var comment = DocumentationComment.Create(
            DocumentationFormat.Markdown,
            """
Parses a widget title.

@param text Input text to parse for [Widget](xref:T:Samples.Docs.Widget).
@returns The parsed title.
@remarks This is culture-invariant.
@see xref:M:Samples.Docs.Widget.GetTitle
""");

        var formatted = DocumentationDisplayFormatter.FormatForMarkdown(comment);

        formatted.ShouldNotBeNull();
        formatted.ShouldContain("Parses a widget title.");
        formatted.ShouldContain("**Parameters**");
        formatted.ShouldContain("`text`");
        formatted.ShouldContain("**Returns**");
        formatted.ShouldContain("The parsed title.");
        formatted.ShouldContain("**Remarks**");
        formatted.ShouldContain("`Widget`");
        formatted.ShouldContain("`Samples.Docs.Widget.GetTitle`");
        formatted.ShouldNotContain("@param");
        formatted.ShouldNotContain("xref:");
    }

    [Fact]
    public void MarkdownDocumentation_WithLinkMode_EmitsRavenDocLinks()
    {
        var comment = DocumentationComment.Create(
            DocumentationFormat.Markdown,
            """
See [Widget](xref:T:Samples.Docs.Widget).

@see xref:M:Samples.Docs.Widget.GetTitle
""");

        var formatted = DocumentationDisplayFormatter.FormatForMarkdown(comment, linkXrefs: true);

        formatted.ShouldNotBeNull();
        formatted.ShouldContain("[Widget](raven-doc:///");
        formatted.ShouldContain("target=T%3ASamples.Docs.Widget");
        formatted.ShouldContain("target=M%3ASamples.Docs.Widget.GetTitle");
    }
}
