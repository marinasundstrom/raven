using System.Collections.Immutable;
using System.Text;
using System.Text.RegularExpressions;
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
        var markdownSections = SplitMarkdownSections(documentation.Body);
        var summary = MarkdownDocumentationStructureExtractor.GetFirstParagraph(markdownSections.Body);
        var additionalBody = MarkdownDocumentationStructureExtractor.GetRemainingParagraphs(markdownSections.Body);

        var returns = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Returns)?.Content;
        var value = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Value)?.Content;
        var remarks = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Remarks)?.Content;
        var example = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Example)?.Content;
        var inheritDocReference = NormalizeReference(documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.InheritDoc)?.Argument);

        if (string.IsNullOrWhiteSpace(returns))
            returns = markdownSections.Returns;

        if (string.IsNullOrWhiteSpace(value))
            value = markdownSections.Value;

        if (string.IsNullOrWhiteSpace(remarks))
            remarks = markdownSections.Remarks;

        if (string.IsNullOrWhiteSpace(example))
            example = markdownSections.Example;

        if (string.IsNullOrWhiteSpace(remarks) && !string.IsNullOrWhiteSpace(additionalBody))
            remarks = additionalBody;

        return new DocumentationStructure(
            SourceFormat: DocumentationFormat.Markdown,
            Summary: summary,
            Body: markdownSections.Body,
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

    private static MarkdownSectionSplit SplitMarkdownSections(string body)
    {
        if (string.IsNullOrWhiteSpace(body))
            return new MarkdownSectionSplit(string.Empty, null, null, null, null);

        var normalizedBody = body.Replace("\r\n", "\n", StringComparison.Ordinal);
        var lines = normalizedBody.Split('\n');
        var bodyBuilder = new StringBuilder();
        var sectionBuilder = new StringBuilder();
        string? currentSection = null;
        string? remarks = null;
        string? returns = null;
        string? value = null;
        string? example = null;

        void FlushSection()
        {
            if (currentSection is null)
                return;

            var content = sectionBuilder.ToString().Trim();
            switch (currentSection)
            {
                case "remarks":
                    remarks ??= EmptyToNull(content);
                    break;
                case "returns":
                    returns ??= EmptyToNull(content);
                    break;
                case "value":
                    value ??= EmptyToNull(content);
                    break;
                case "example":
                    example ??= EmptyToNull(content);
                    break;
            }

            sectionBuilder.Clear();
            currentSection = null;
        }

        foreach (var line in lines)
        {
            var headingSection = TryGetRecognizedHeadingSection(line);
            if (headingSection is not null)
            {
                FlushSection();
                currentSection = headingSection;
                continue;
            }

            if (currentSection is not null)
            {
                sectionBuilder.AppendLine(line);
            }
            else
            {
                bodyBuilder.AppendLine(line);
            }
        }

        FlushSection();

        return new MarkdownSectionSplit(
            Body: bodyBuilder.ToString().Trim(),
            Returns: returns,
            Value: value,
            Remarks: remarks,
            Example: example);
    }

    private static string? TryGetRecognizedHeadingSection(string line)
    {
        var match = Regex.Match(line, @"^\s{0,3}#{1,6}\s+(?<name>.+?)\s*$");
        if (!match.Success)
            return null;

        var heading = match.Groups["name"].Value.Trim();
        return heading.ToLowerInvariant() switch
        {
            "remarks" => "remarks",
            "returns" => "returns",
            "value" => "value",
            "example" or "examples" => "example",
            _ => null
        };
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

    private sealed record MarkdownSectionSplit(
        string Body,
        string? Returns,
        string? Value,
        string? Remarks,
        string? Example);
}
