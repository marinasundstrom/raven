using System.Text.Json;
using System.Text.RegularExpressions;

using Raven.CodeAnalysis.Documentation;

namespace Raven.LanguageServer;

internal static class DocumentationMarkdownFormatter
{
    public static string? FormatForEditor(DocumentationComment? documentation, bool linkXrefs = true)
    {
        var formatted = DocumentationDisplayFormatter.FormatForMarkdown(documentation, linkXrefs);
        return RewriteDocumentationLinksForEditor(formatted);
    }

    private static string? RewriteDocumentationLinksForEditor(string? markdown)
    {
        if (string.IsNullOrWhiteSpace(markdown))
            return markdown;

        return Regex.Replace(
            markdown,
            @"\[(?<label>[^\]]+)\]\((?<uri>raven-doc:[^)]+)\)",
            static match =>
            {
                var label = match.Groups["label"].Value;
                var uri = match.Groups["uri"].Value;
                var commandArguments = JsonSerializer.Serialize(new[] { uri });
                var commandUri = $"command:raven.openDocumentation?{Uri.EscapeDataString(commandArguments)}";
                return $"[{label}]({commandUri})";
            });
    }
}
