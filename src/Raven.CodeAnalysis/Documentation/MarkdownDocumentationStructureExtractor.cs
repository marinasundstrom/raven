using System.Collections.Immutable;
using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis.Documentation;

public static class MarkdownDocumentationStructureExtractor
{
    public static MarkdownDocumentationStructure Extract(DocumentationComment? documentation)
    {
        if (documentation is null || documentation.Format != DocumentationFormat.Markdown)
        {
            return new MarkdownDocumentationStructure(
                Summary: string.Empty,
                Body: string.Empty,
                AdditionalBody: string.Empty,
                Returns: null,
                Value: null,
                Remarks: null,
                Example: null,
                InheritDocReference: null,
                TypeParameters: ImmutableArray<MarkdownDocumentationEntry>.Empty,
                Parameters: ImmutableArray<MarkdownDocumentationEntry>.Empty,
                Exceptions: ImmutableArray<MarkdownDocumentationEntry>.Empty,
                See: ImmutableArray<MarkdownDocumentationEntry>.Empty,
                SeeAlso: ImmutableArray<MarkdownDocumentationEntry>.Empty);
        }

        var summary = GetFirstParagraph(documentation.Body);
        var additionalBody = GetRemainingParagraphs(documentation.Body);

        var returns = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Returns)?.Content;
        var value = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Value)?.Content;
        var remarks = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Remarks)?.Content;
        var example = documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.Example)?.Content;
        var inheritDocReference = NormalizeReference(documentation.Tags.FirstOrDefault(static tag => tag.Kind == DocumentationTagKind.InheritDoc)?.Argument);

        if (string.IsNullOrWhiteSpace(remarks) && !string.IsNullOrWhiteSpace(additionalBody))
            remarks = additionalBody;

        return new MarkdownDocumentationStructure(
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

    private static ImmutableArray<MarkdownDocumentationEntry> ConvertEntries(IEnumerable<DocumentationTag> tags)
    {
        return tags
            .Select(static tag => new MarkdownDocumentationEntry(
                Name: EmptyToNull(tag.Argument),
                Reference: NormalizeReference(tag.Argument),
                Content: tag.Content))
            .ToImmutableArray();
    }

    private static string GetFirstParagraph(string body)
    {
        if (string.IsNullOrWhiteSpace(body))
            return string.Empty;

        var paragraphs = SplitParagraphs(body);
        return paragraphs.Count > 0 ? paragraphs[0] : string.Empty;
    }

    private static string GetRemainingParagraphs(string body)
    {
        if (string.IsNullOrWhiteSpace(body))
            return string.Empty;

        var paragraphs = SplitParagraphs(body);
        if (paragraphs.Count <= 1)
            return string.Empty;

        return string.Join(Environment.NewLine + Environment.NewLine, paragraphs.Skip(1));
    }

    private static List<string> SplitParagraphs(string body)
    {
        return Regex.Split(body.Trim(), @"(?:\r?\n){2,}")
            .Select(static paragraph => paragraph.Trim())
            .Where(static paragraph => paragraph.Length > 0)
            .ToList();
    }

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
