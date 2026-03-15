using System.Collections.Immutable;
using System.Xml.Linq;

namespace Raven.CodeAnalysis.Documentation;

public static class DocumentationStructureExtractor
{
    public static DocumentationStructure Extract(DocumentationComment? documentation)
    {
        if (documentation is null)
            return Empty(DocumentationFormat.Markdown);

        return documentation.Format switch
        {
            DocumentationFormat.Markdown => FromMarkdown(documentation),
            DocumentationFormat.Xml => FromXml(documentation),
            _ => Empty(documentation.Format)
        };
    }

    private static DocumentationStructure FromMarkdown(DocumentationComment documentation)
    {
        var summary = MarkdownDocumentationStructureExtractor.GetFirstParagraph(documentation.Body);
        var additionalBody = MarkdownDocumentationStructureExtractor.GetRemainingParagraphs(documentation.Body);

        var returns = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Returns)?.Content;
        var value = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Value)?.Content;
        var remarks = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Remarks)?.Content;
        var example = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Example)?.Content;
        var inheritDocReference = NormalizeReference(documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.InheritDoc)?.Argument);

        if (string.IsNullOrWhiteSpace(remarks) && !string.IsNullOrWhiteSpace(additionalBody))
            remarks = additionalBody;

        return new DocumentationStructure(
            SourceFormat: DocumentationFormat.Markdown,
            Summary: summary,
            Body: documentation.Body,
            AdditionalBody: additionalBody,
            Returns: EmptyToNull(returns),
            Value: EmptyToNull(value),
            Remarks: EmptyToNull(remarks),
            Example: EmptyToNull(example),
            InheritDocReference: EmptyToNull(inheritDocReference),
            TypeParameters: ConvertEntries(documentation.Tags.Where(static tag => tag.Kind == DocumentationTagKind.TypeParam)),
            Parameters: ConvertEntries(documentation.Tags.Where(static tag => tag.Kind == DocumentationTagKind.Param)),
            Exceptions: ConvertEntries(documentation.Tags.Where(static tag => tag.Kind == DocumentationTagKind.Exception)),
            See: ConvertEntries(documentation.Tags.Where(static tag => tag.Kind == DocumentationTagKind.See)),
            SeeAlso: ConvertEntries(documentation.Tags.Where(static tag => tag.Kind == DocumentationTagKind.SeeAlso)));
    }

    private static DocumentationStructure FromXml(DocumentationComment documentation)
    {
        try
        {
            var document = XDocument.Parse($"<doc>{documentation.Content}</doc>", LoadOptions.PreserveWhitespace);
            var root = document.Root;
            if (root is null)
                return Empty(DocumentationFormat.Xml);

            var summary = XmlDocumentationTextExtractor.RenderElementBody(root.Element("summary"));
            var remarks = XmlDocumentationTextExtractor.RenderElementBody(root.Element("remarks"));
            var returns = XmlDocumentationTextExtractor.RenderElementBody(root.Element("returns"));
            var value = XmlDocumentationTextExtractor.RenderElementBody(root.Element("value"));
            var example = XmlDocumentationTextExtractor.RenderElementBody(root.Element("example"));

            return new DocumentationStructure(
                SourceFormat: DocumentationFormat.Xml,
                Summary: summary,
                Body: summary,
                AdditionalBody: string.Empty,
                Returns: EmptyToNull(returns),
                Value: EmptyToNull(value),
                Remarks: EmptyToNull(remarks),
                Example: EmptyToNull(example),
                InheritDocReference: NormalizeReference((string?)root.Element("inheritdoc")?.Attribute("cref")),
                TypeParameters: root.Elements("typeparam").Select(ToEntry).ToImmutableArray(),
                Parameters: root.Elements("param").Select(ToEntry).ToImmutableArray(),
                Exceptions: root.Elements("exception").Select(ToEntry).ToImmutableArray(),
                See: root.Elements("see").Select(ToEntry).ToImmutableArray(),
                SeeAlso: root.Elements("seealso").Select(ToEntry).ToImmutableArray());
        }
        catch
        {
            return Empty(DocumentationFormat.Xml);
        }
    }

    private static DocumentationEntry ToEntry(XElement element)
        => new(
            Name: EmptyToNull((string?)element.Attribute("name")),
            Reference: NormalizeReference((string?)element.Attribute("cref")),
            Content: XmlDocumentationTextExtractor.RenderElementBody(element));

    private static ImmutableArray<DocumentationEntry> ConvertEntries(IEnumerable<DocumentationTag> tags)
    {
        return tags
            .Select(static tag => new DocumentationEntry(
                Name: EmptyToNull(tag.Argument),
                Reference: NormalizeReference(tag.Argument),
                Content: tag.Content))
            .ToImmutableArray();
    }

    private static DocumentationStructure Empty(DocumentationFormat format)
        => new(
            SourceFormat: format,
            Summary: string.Empty,
            Body: string.Empty,
            AdditionalBody: string.Empty,
            Returns: null,
            Value: null,
            Remarks: null,
            Example: null,
            InheritDocReference: null,
            TypeParameters: ImmutableArray<DocumentationEntry>.Empty,
            Parameters: ImmutableArray<DocumentationEntry>.Empty,
            Exceptions: ImmutableArray<DocumentationEntry>.Empty,
            See: ImmutableArray<DocumentationEntry>.Empty,
            SeeAlso: ImmutableArray<DocumentationEntry>.Empty);

    private static string? NormalizeReference(string? reference)
    {
        if (string.IsNullOrWhiteSpace(reference))
            return null;

        return reference.StartsWith("xref:", StringComparison.OrdinalIgnoreCase)
            ? reference["xref:".Length..].Trim()
            : reference.Trim();
    }

    private static string? EmptyToNull(string? value)
        => string.IsNullOrWhiteSpace(value) ? null : value;
}
