using System.Text.Json;

internal sealed record PageFrontMatter(string Content, string? Title = null, bool? Toc = null, string Layout = "docs")
{
    // A deliberately small, strict YAML scalar subset. Unknown options fail so
    // misspelled publishing controls cannot silently change the rendered result.
    public static PageFrontMatter Parse(string source)
    {
        source = source.Replace("\r\n", "\n");
        if (!source.StartsWith("---\n", StringComparison.Ordinal)) return new(source);
        var end = source.IndexOf("\n---\n", 4, StringComparison.Ordinal);
        if (end < 0) throw new InvalidOperationException("Unclosed page front matter.");
        string? title = null;
        bool? toc = null;
        var layout = "docs";
        var keys = new HashSet<string>(StringComparer.Ordinal);
        foreach (var line in source[4..end].Split('\n'))
        {
            if (string.IsNullOrWhiteSpace(line) || line.TrimStart().StartsWith('#')) continue;
            var colon = line.IndexOf(':');
            if (colon < 1) throw new InvalidOperationException("Expected front-matter key: value.");
            var key = line[..colon].Trim();
            var value = line[(colon + 1)..].Trim();
            if (!keys.Add(key)) throw new InvalidOperationException($"Duplicate front-matter key: {key}");
            if (value.StartsWith('"')) value = JsonSerializer.Deserialize<string>(value)!;
            else if (value.StartsWith('\'') && value.EndsWith('\'')) value = value[1..^1].Replace("''", "'");
            switch (key)
            {
                case "title" when !string.IsNullOrWhiteSpace(value): title = value; break;
                case "toc" when value is "true" or "false": toc = value == "true"; break;
                case "layout" when value is "docs" or "landing": layout = value; break;
                default: throw new InvalidOperationException($"Unsupported front matter: {key}: {value}");
            }
        }
        return new(source[(end + 5)..], title, toc, layout);
    }
}
