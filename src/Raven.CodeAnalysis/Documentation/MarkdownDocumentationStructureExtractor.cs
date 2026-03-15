using System.Collections.Immutable;
using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis.Documentation;

public static class MarkdownDocumentationStructureExtractor
{
    internal static string GetFirstParagraph(string body)
    {
        if (string.IsNullOrWhiteSpace(body))
            return string.Empty;

        var paragraphs = SplitParagraphs(body);
        return paragraphs.Count > 0 ? paragraphs[0] : string.Empty;
    }

    internal static string GetRemainingParagraphs(string body)
    {
        if (string.IsNullOrWhiteSpace(body))
            return string.Empty;

        var paragraphs = SplitParagraphs(body);
        if (paragraphs.Count <= 1)
            return string.Empty;

        return string.Join(Environment.NewLine + Environment.NewLine, paragraphs.Skip(1));
    }

    public static MarkdownDocumentationStructure Extract(DocumentationComment? documentation)
    {
        var structure = DocumentationStructureExtractor.Extract(documentation);
        return new MarkdownDocumentationStructure(
            Summary: structure.Summary,
            Body: structure.Body,
            AdditionalBody: structure.AdditionalBody,
            Returns: structure.Returns,
            Value: structure.Value,
            Remarks: structure.Remarks,
            Example: structure.Example,
            InheritDocReference: structure.InheritDocReference,
            TypeParameters: structure.TypeParameters.Select(static e => new MarkdownDocumentationEntry(e.Name, e.Reference, e.Content)).ToImmutableArray(),
            Parameters: structure.Parameters.Select(static e => new MarkdownDocumentationEntry(e.Name, e.Reference, e.Content)).ToImmutableArray(),
            Exceptions: structure.Exceptions.Select(static e => new MarkdownDocumentationEntry(e.Name, e.Reference, e.Content)).ToImmutableArray(),
            See: structure.See.Select(static e => new MarkdownDocumentationEntry(e.Name, e.Reference, e.Content)).ToImmutableArray(),
            SeeAlso: structure.SeeAlso.Select(static e => new MarkdownDocumentationEntry(e.Name, e.Reference, e.Content)).ToImmutableArray());
    }

    private static List<string> SplitParagraphs(string body)
    {
        return Regex.Split(body.Trim(), @"(?:\r?\n){2,}")
            .Select(static paragraph => paragraph.Trim())
            .Where(static paragraph => paragraph.Length > 0)
            .ToList();
    }

}
