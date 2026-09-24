using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

internal static class DocumentationTableOfContents
{
    internal static List<DocumentationNavigationItem> Load(
        string path, string root, Func<string, string, string> resolvePage)
    {
        var activeFiles = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .IgnoreUnmatchedProperties()
            .Build();
        return Read(path);

        List<DocumentationNavigationItem> Read(string tocPath)
        {
            tocPath = Path.GetFullPath(tocPath);
            if (!activeFiles.Add(tocPath))
                throw new InvalidOperationException($"Circular table of contents include: {tocPath}");
            try
            {
                var items = deserializer.Deserialize<List<TocItem>>(File.ReadAllText(tocPath)) ?? [];
                return items.Select(item => Convert(item, Path.GetDirectoryName(tocPath)!)).ToList();
            }
            finally
            {
                activeFiles.Remove(tocPath);
            }
        }

        DocumentationNavigationItem Convert(TocItem item, string directory)
        {
            if (string.IsNullOrWhiteSpace(item.Name))
                throw new InvalidOperationException("Each toc.yml entry must have a name.");
            var children = item.Items.Select(child => Convert(child, directory)).ToList();
            var href = item.TocHref ?? item.Href;
            string? url = null;
            var api = false;
            if (!string.IsNullOrWhiteSpace(href))
            {
                if (href.StartsWith('/') || href.StartsWith('#') || Uri.TryCreate(href, UriKind.Absolute, out _))
                    url = href;
                else
                {
                    var suffixIndex = href.IndexOfAny(['#', '?']);
                    var localPath = suffixIndex < 0 ? href : href[..suffixIndex];
                    var suffix = suffixIndex < 0 ? "" : href[suffixIndex..];
                    var source = Path.GetFullPath(Uri.UnescapeDataString(localPath), directory);
                    var relative = Path.GetRelativePath(root, source).Replace('\\', '/').TrimEnd('/');
                    if (relative is "api" or "api/index.html" or "api/toc.yml")
                        api = true;
                    else if ((source.EndsWith(".md", StringComparison.OrdinalIgnoreCase) || source.EndsWith(".html", StringComparison.OrdinalIgnoreCase)))
                        url = resolvePage(source, item.Name) + suffix;
                    else if (source.EndsWith(".yml", StringComparison.OrdinalIgnoreCase))
                        children.AddRange(Read(source));
                    else if (Directory.Exists(source) && File.Exists(Path.Combine(source, "toc.yml")))
                        children.AddRange(Read(Path.Combine(source, "toc.yml")));
                    else
                        url = relative + suffix;
                }
            }
            if (item.TopicHref is { } topic)
            {
                var topicItem = Convert(new TocItem { Name = item.Name, Href = topic }, directory);
                url = topicItem.Url;
            }
            return new DocumentationNavigationItem(item.Name, url, children, api);
        }
    }

    private sealed class TocItem
    {
        public string Name { get; set; } = "";
        public string? Href { get; set; }
        public string? TocHref { get; set; }
        public string? TopicHref { get; set; }
        public List<TocItem> Items { get; set; } = [];
    }
}
