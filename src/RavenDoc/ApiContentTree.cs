/// <summary>Authored additions keyed by exact documentation IDs, independent of file layout.</summary>
internal sealed class ApiContentTree
{
    private readonly Dictionary<string, (string Path, string Markdown)> entries = new(StringComparer.Ordinal);
    private readonly HashSet<string> used = new(StringComparer.Ordinal);

    public ApiContentTree(string? directory)
    {
        if (directory is null) return;
        if (!Directory.Exists(directory))
            throw new InvalidOperationException($"API content directory does not exist: {directory}");
        foreach (var path in Directory.EnumerateFiles(directory, "*.md", SearchOption.AllDirectories).Order(StringComparer.Ordinal))
        {
            var page = PageFrontMatter.Parse(File.ReadAllText(path));
            if (string.IsNullOrWhiteSpace(page.Uid))
                throw new InvalidOperationException($"API content needs a uid: {path}");
            if (!entries.TryAdd(page.Uid, (path, page.Content)))
                throw new InvalidOperationException($"Duplicate API content uid '{page.Uid}': {path}");
        }
    }

    public string Merge(string uid, string? generated)
    {
        if (!entries.TryGetValue(uid, out var entry)) return generated ?? "";
        used.Add(uid);
        return (generated ?? "").TrimEnd() + "\n\n" + entry.Markdown;
    }

    public void Validate()
    {
        var unmatched = entries.Keys.Except(used).Order(StringComparer.Ordinal).ToArray();
        if (unmatched.Length > 0)
            throw new InvalidOperationException("API content IDs did not match rendered symbols: " + string.Join(", ", unmatched));
    }
}
