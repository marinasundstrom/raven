using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Documentation;

internal static class MarkdownDocumentationFrontMatter
{
    public static string Write(string markdownBody, params (string Key, string Value)[] entries)
    {
        var lines = new List<string> { "---" };
        foreach (var (key, value) in entries)
        {
            if (string.IsNullOrWhiteSpace(key) || string.IsNullOrWhiteSpace(value))
                continue;

            lines.Add($"{key}: {value}");
        }

        lines.Add("---");
        lines.Add(string.Empty);
        lines.Add(markdownBody);
        return string.Join(Environment.NewLine, lines);
    }

    public static ParsedMarkdownDocument Parse(string content)
    {
        if (string.IsNullOrEmpty(content) || !content.StartsWith("---", StringComparison.Ordinal))
            return new ParsedMarkdownDocument(content, ImmutableDictionary<string, string>.Empty);

        var normalized = content.Replace("\r\n", "\n", StringComparison.Ordinal);
        if (!normalized.StartsWith("---\n", StringComparison.Ordinal))
            return new ParsedMarkdownDocument(content, ImmutableDictionary<string, string>.Empty);

        var endMarker = normalized.IndexOf("\n---\n", 4, StringComparison.Ordinal);
        if (endMarker < 0)
            return new ParsedMarkdownDocument(content, ImmutableDictionary<string, string>.Empty);

        var frontMatterBlock = normalized.Substring(4, endMarker - 4);
        var body = normalized[(endMarker + 5)..];
        var builder = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.OrdinalIgnoreCase);

        foreach (var rawLine in frontMatterBlock.Split('\n'))
        {
            var line = rawLine.Trim();
            if (line.Length == 0 || line.StartsWith('#'))
                continue;

            var separator = line.IndexOf(':');
            if (separator <= 0 || separator == line.Length - 1)
                continue;

            var key = line[..separator].Trim();
            var value = line[(separator + 1)..].Trim();
            if (key.Length == 0 || value.Length == 0)
                continue;

            builder[key] = value;
        }

        return new ParsedMarkdownDocument(body, builder.ToImmutable());
    }

    internal readonly record struct ParsedMarkdownDocument(
        string Body,
        ImmutableDictionary<string, string> FrontMatter);
}
