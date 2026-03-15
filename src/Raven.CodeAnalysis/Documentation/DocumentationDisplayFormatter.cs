using System.Text;
using System.Xml.Linq;
using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis.Documentation;

public static class DocumentationDisplayFormatter
{
    public static string? FormatForMarkdown(DocumentationComment? documentation, bool linkXrefs = false)
    {
        if (documentation is null || string.IsNullOrWhiteSpace(documentation.Content))
            return null;

        return documentation.Format switch
        { 
            DocumentationFormat.Markdown => FormatMarkdownForDisplay(documentation, linkXrefs),
            DocumentationFormat.Xml => FormatXmlForMarkdown(documentation.Content),
            _ => documentation.Content.Trim()
        };
    }

    private static string FormatMarkdownForDisplay(DocumentationComment documentation, bool linkXrefs)
    {
        var sections = new List<string>();
        if (!string.IsNullOrWhiteSpace(documentation.Body))
            sections.Add(RewriteXrefsForDisplay(documentation.Body.Trim(), linkXrefs));

        AppendTagSection(sections, "Type Parameters", documentation.Tags.Where(static t => t.Kind == DocumentationTagKind.TypeParam), includeArgument: true, linkXrefs);
        AppendTagSection(sections, "Parameters", documentation.Tags.Where(static t => t.Kind == DocumentationTagKind.Param), includeArgument: true, linkXrefs);
        AppendSingleSection(sections, "Returns", documentation.Tags.FirstOrDefault(static t => t.Kind == DocumentationTagKind.Returns), linkXrefs);
        AppendSingleSection(sections, "Value", documentation.Tags.FirstOrDefault(static t => t.Kind == DocumentationTagKind.Value), linkXrefs);
        AppendSingleSection(sections, "Remarks", documentation.Tags.FirstOrDefault(static t => t.Kind == DocumentationTagKind.Remarks), linkXrefs);
        AppendSingleSection(sections, "Example", documentation.Tags.FirstOrDefault(static t => t.Kind == DocumentationTagKind.Example), linkXrefs);
        AppendTagSection(sections, "Exceptions", documentation.Tags.Where(static t => t.Kind == DocumentationTagKind.Exception), includeArgument: true, linkXrefs);
        AppendTagSection(sections, "See Also", documentation.Tags.Where(static t => t.Kind is DocumentationTagKind.See or DocumentationTagKind.SeeAlso), includeArgument: true, linkXrefs);
        AppendSingleSection(sections, "Inherited Documentation", documentation.Tags.FirstOrDefault(static t => t.Kind == DocumentationTagKind.InheritDoc), linkXrefs);

        return string.Join("\n\n", sections.Where(static s => !string.IsNullOrWhiteSpace(s)));
    }

    private static string FormatXmlForMarkdown(string content)
    {
        if (!TryRenderXmlDocumentation(content, out var rendered))
            return $"```xml\n{content.Trim()}\n```";

        return rendered;
    }

    private static bool TryRenderXmlDocumentation(string content, out string rendered)
    {
        rendered = string.Empty;

        try
        {
            var document = XDocument.Parse($"<doc>{content}</doc>", LoadOptions.PreserveWhitespace);
            var root = document.Root;
            if (root is null)
                return false;

            var sections = new List<string>();

            var summary = GetFirstElementBody(root, "summary");
            if (!string.IsNullOrWhiteSpace(summary))
                sections.Add(summary);

            AppendNamedSection(root, "remarks", "Remarks", sections);
            AppendNamedSection(root, "returns", "Returns", sections);
            AppendNamedSection(root, "value", "Value", sections);
            AppendNamedSection(root, "example", "Example", sections);

            var typeParameters = root.Elements("typeparam")
                .Select(static element =>
                {
                    var name = (string?)element.Attribute("name");
                    var body = RenderBlockContent(element);
                    return string.IsNullOrWhiteSpace(name) || string.IsNullOrWhiteSpace(body)
                        ? null
                        : $"- `{name}`: {body}";
                })
                .Where(static entry => !string.IsNullOrWhiteSpace(entry))
                .Cast<string>()
                .ToList();

            if (typeParameters.Count > 0)
                sections.Add($"**Type Parameters**\n\n{string.Join("\n", typeParameters)}");

            var parameters = root.Elements("param")
                .Select(static element =>
                {
                    var name = (string?)element.Attribute("name");
                    var body = RenderBlockContent(element);
                    return string.IsNullOrWhiteSpace(name) || string.IsNullOrWhiteSpace(body)
                        ? null
                        : $"- `{name}`: {body}";
                })
                .Where(static entry => !string.IsNullOrWhiteSpace(entry))
                .Cast<string>()
                .ToList();

            if (parameters.Count > 0)
                sections.Add($"**Parameters**\n\n{string.Join("\n", parameters)}");

            var exceptions = root.Elements("exception")
                .Select(static element =>
                {
                    var cref = RenderCref((string?)element.Attribute("cref"));
                    var body = RenderBlockContent(element);
                    if (string.IsNullOrWhiteSpace(cref) && string.IsNullOrWhiteSpace(body))
                        return null;

                    return string.IsNullOrWhiteSpace(body)
                        ? $"- {cref}"
                        : $"- {cref}: {body}";
                })
                .Where(static entry => !string.IsNullOrWhiteSpace(entry))
                .Cast<string>()
                .ToList();

            if (exceptions.Count > 0)
                sections.Add($"**Exceptions**\n\n{string.Join("\n", exceptions)}");

            if (sections.Count == 0)
                return false;

            rendered = string.Join("\n\n", sections);
            return true;
        }
        catch
        {
            return false;
        }
    }

    private static void AppendNamedSection(XElement root, string elementName, string heading, List<string> sections)
    {
        var body = GetFirstElementBody(root, elementName);
        if (!string.IsNullOrWhiteSpace(body))
            sections.Add($"**{heading}**\n\n{body}");
    }

    private static string? GetFirstElementBody(XElement root, string elementName)
    {
        var element = root.Element(elementName);
        if (element is null)
            return null;

        var body = RenderBlockContent(element);
        return string.IsNullOrWhiteSpace(body) ? null : body;
    }

    private static string RenderBlockContent(XElement element)
    {
        var paragraphs = new List<string>();
        var inlineBuffer = new StringBuilder();

        void FlushInline()
        {
            var inline = NormalizeWhitespace(inlineBuffer.ToString());
            if (!string.IsNullOrWhiteSpace(inline))
                paragraphs.Add(inline);

            inlineBuffer.Clear();
        }

        foreach (var node in element.Nodes())
        {
            if (node is XElement child && child.Name.LocalName == "para")
            {
                FlushInline();
                var paragraph = RenderBlockContent(child);
                if (!string.IsNullOrWhiteSpace(paragraph))
                    paragraphs.Add(paragraph);
                continue;
            }

            inlineBuffer.Append(RenderInlineNode(node));
        }

        FlushInline();
        return string.Join("\n\n", paragraphs);
    }

    private static string RenderInlineNode(XNode node)
    {
        return node switch
        {
            XText text => text.Value,
            XElement element => RenderInlineElement(element),
            _ => string.Empty
        };
    }

    private static string RenderInlineElement(XElement element)
    {
        var body = string.Concat(element.Nodes().Select(RenderInlineNode));

        return element.Name.LocalName switch
        {
            "c" => $"`{NormalizeWhitespace(body)}`",
            "code" => $"\n```text\n{body.Trim()}\n```\n",
            "see" => RenderSeeElement(element),
            "seealso" => RenderSeeElement(element),
            "paramref" => $"`{(string?)element.Attribute("name") ?? NormalizeWhitespace(body)}`",
            "typeparamref" => $"`{(string?)element.Attribute("name") ?? NormalizeWhitespace(body)}`",
            "br" => "\n",
            _ => body
        };
    }

    private static string RenderSeeElement(XElement element)
    {
        var cref = (string?)element.Attribute("cref");
        if (!string.IsNullOrWhiteSpace(cref))
            return RenderCref(cref);

        var href = (string?)element.Attribute("href");
        var body = NormalizeWhitespace(string.Concat(element.Nodes().Select(RenderInlineNode)));
        if (!string.IsNullOrWhiteSpace(href))
            return string.IsNullOrWhiteSpace(body) ? href : $"[{body}]({href})";

        return body;
    }

    private static string RenderCref(string? cref)
    {
        if (string.IsNullOrWhiteSpace(cref))
            return string.Empty;

        var value = cref.Trim();
        var colon = value.IndexOf(':');
        if (colon >= 0 && colon < value.Length - 1)
            value = value[(colon + 1)..];

        return $"`{value}`";
    }

    private static string NormalizeWhitespace(string text)
    {
        return System.Text.RegularExpressions.Regex.Replace(text, "\\s+", " ").Trim();
    }

    private static void AppendSingleSection(List<string> sections, string heading, DocumentationTag? tag, bool linkXrefs)
    {
        if (tag is null || string.IsNullOrWhiteSpace(tag.Content))
            return;

        sections.Add($"**{heading}**\n\n{RewriteXrefsForDisplay(tag.Content.Trim(), linkXrefs)}");
    }

    private static void AppendTagSection(List<string> sections, string heading, IEnumerable<DocumentationTag> tags, bool includeArgument, bool linkXrefs)
    {
        var entries = tags
            .Select(tag =>
            {
                var label = includeArgument && !string.IsNullOrWhiteSpace(tag.Argument)
                    ? FormatTagArgument(tag.Argument!, linkXrefs)
                    : null;

                if (string.IsNullOrWhiteSpace(tag.Content))
                    return label is null ? null : $"- {label}";

                var content = RewriteXrefsForDisplay(tag.Content.Trim(), linkXrefs);
                return label is null
                    ? $"- {content}"
                    : $"- {label}: {content}";
            })
            .Where(static entry => !string.IsNullOrWhiteSpace(entry))
            .Cast<string>()
            .ToList();

        if (entries.Count == 0)
            return;

        sections.Add($"**{heading}**\n\n{string.Join("\n", entries)}");
    }

    private static string FormatTagArgument(string argument, bool linkXrefs)
    {
        if (argument.StartsWith("xref:", StringComparison.OrdinalIgnoreCase))
            return FormatXrefTarget(argument["xref:".Length..], linkXrefs);

        return $"`{argument}`";
    }

    private static string RewriteXrefsForDisplay(string markdown, bool linkXrefs)
    {
        if (string.IsNullOrWhiteSpace(markdown))
            return markdown;

        var rewrittenLinks = Regex.Replace(
            markdown,
            @"\[(?<label>[^\]]+)\]\(xref:(?<target>[^)]+)\)",
            match =>
            {
                var label = match.Groups["label"].Value;
                var target = match.Groups["target"].Value;
                return linkXrefs
                    ? $"[{label}]({CreateDocumentationUri(target, label)})"
                    : $"`{label}`";
            });

        return Regex.Replace(
            rewrittenLinks,
            @"(?<!\()xref:(?<target>[A-Z]:[^\s)]+)",
            match => FormatXrefTarget(match.Groups["target"].Value, linkXrefs));
    }

    private static string FormatXrefTarget(string target, bool linkXrefs)
    {
        var value = target.Trim();
        var colon = value.IndexOf(':');
        if (colon >= 0 && colon < value.Length - 1)
            value = value[(colon + 1)..];

        return linkXrefs
            ? $"[{value}]({CreateDocumentationUri(target.Trim(), value)})"
            : $"`{value}`";
    }

    private static string CreateDocumentationUri(string target, string label)
    {
        var encodedTarget = Uri.EscapeDataString(target);
        var encodedLabel = Uri.EscapeDataString(label);
        return $"raven-doc:///xref.md?target={encodedTarget}&label={encodedLabel}";
    }
}
