using System.Text;
using System.Xml.Linq;

namespace Raven.CodeAnalysis.Documentation;

internal static class XmlDocumentationTextExtractor
{
    public static string RenderElementBody(XElement? element)
    {
        if (element is null)
            return string.Empty;

        return NormalizeWhitespace(RenderNodes(element.Nodes()));
    }

    private static string RenderNodes(IEnumerable<XNode> nodes)
    {
        var builder = new StringBuilder();
        foreach (var node in nodes)
            RenderNode(node, builder);

        return builder.ToString();
    }

    private static void RenderNode(XNode node, StringBuilder builder)
    {
        switch (node)
        {
            case XText text:
                builder.Append(text.Value);
                break;
            case XElement element:
                RenderElement(element, builder);
                break;
        }
    }

    private static void RenderElement(XElement element, StringBuilder builder)
    {
        switch (element.Name.LocalName)
        {
            case "see":
            case "seealso":
                var cref = NormalizeCref(element.Attribute("cref")?.Value);
                if (!string.IsNullOrWhiteSpace(cref))
                {
                    builder.Append(cref);
                    return;
                }

                var href = element.Attribute("href")?.Value;
                if (!string.IsNullOrWhiteSpace(href))
                {
                    builder.Append(href);
                    return;
                }

                builder.Append(RenderNodes(element.Nodes()));
                return;
            case "paramref":
            case "typeparamref":
                builder.Append(element.Attribute("name")?.Value);
                return;
            case "c":
            case "code":
            case "para":
                builder.Append(RenderNodes(element.Nodes()));
                return;
            case "br":
                builder.AppendLine();
                return;
            default:
                builder.Append(RenderNodes(element.Nodes()));
                return;
        }
    }

    private static string NormalizeWhitespace(string text)
        => string.Join(" ", text
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Split(['\n', '\r'], StringSplitOptions.RemoveEmptyEntries)
            .Select(static line => line.Trim())
            .Where(static line => line.Length > 0));

    private static string NormalizeCref(string? cref)
    {
        if (string.IsNullOrWhiteSpace(cref))
            return string.Empty;

        var value = cref.Trim();
        var colon = value.IndexOf(':');
        return colon >= 0 && colon < value.Length - 1
            ? value[(colon + 1)..]
            : value;
    }
}
