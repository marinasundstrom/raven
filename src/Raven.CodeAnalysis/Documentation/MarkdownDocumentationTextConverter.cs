using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis.Documentation;

public static class MarkdownDocumentationTextConverter
{
    public static string ToPlainText(string? markdown)
    {
        if (string.IsNullOrWhiteSpace(markdown))
            return string.Empty;

        var text = markdown.Replace("\r\n", "\n", StringComparison.Ordinal).Trim();

        text = Regex.Replace(text, @"^\s{0,3}#{1,6}\s*", string.Empty, RegexOptions.Multiline);
        text = Regex.Replace(text, @"^\s*[-*+]\s+", string.Empty, RegexOptions.Multiline);
        text = Regex.Replace(text, @"\[(?<label>[^\]]+)\]\((?<target>[^)]+)\)", "${label}");
        text = Regex.Replace(text, @"`(?<code>[^`]+)`", "${code}");
        text = Regex.Replace(text, @"\*\*(?<text>[^*]+)\*\*", "${text}");
        text = Regex.Replace(text, @"\*(?<text>[^*]+)\*", "${text}");
        text = Regex.Replace(text, @"_(?<text>[^_]+)_", "${text}");
        text = Regex.Replace(text, @"\s+\n", "\n");

        return text.Trim();
    }
}
