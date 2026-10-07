using System.Net;
using System.Text.Json;
using System.Text.RegularExpressions;

/// <summary>Indexes authored articles and generated API pages in a published site.</summary>
internal static class DocumentationSiteSearch
{
    public static void Apply(string root, bool enabled)
    {
        var entries = new List<SearchEntry>();
        foreach (var path in Directory.EnumerateFiles(root, "*.html", SearchOption.AllDirectories).Order(StringComparer.Ordinal))
        {
            var html = File.ReadAllText(path);
            // Re-finalizing a combined site replaces controls rather than duplicating them.
            html = Regex.Replace(html, @"<!-- ravendoc-search -->.*?<!-- /ravendoc-search -->", "", RegexOptions.Singleline);
            var article = Regex.Match(html, @"<article\b[^>]*>(.*?)</article>", RegexOptions.Singleline | RegexOptions.IgnoreCase);
            if (enabled && article.Success)
            {
                var title = PlainText(Regex.Match(html, @"<title\b[^>]*>(.*?)</title>", RegexOptions.Singleline | RegexOptions.IgnoreCase).Groups[1].Value);
                entries.Add(new(Path.GetRelativePath(root, path).Replace('\\', '/'), title, PlainText(article.Groups[1].Value)));
                var relativeRoot = Path.GetRelativePath(Path.GetDirectoryName(path)!, root).Replace('\\', '/');
                var script = WebUtility.HtmlEncode(relativeRoot + "/search.js");
                var control = $$"""
                    <!-- ravendoc-search --><div class="site-search">
                      <button type="button" class="site-search-toggle" aria-label="Search site" aria-expanded="false" aria-controls="site-search-panel">
                        <svg viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="m16 16 5 5"/></svg>
                      </button>
                      <section id="site-search-panel" class="site-search-panel" aria-label="Site search" hidden>
                        <label for="site-search-query">Search documentation and APIs</label>
                        <input id="site-search-query" type="search" autocomplete="off" />
                        <p class="site-search-status" role="status" aria-live="polite"></p>
                        <ol class="site-search-results"></ol>
                      </section>
                    </div><script type="module" src="{{script}}"></script><!-- /ravendoc-search -->
                    """;
                html = html.Replace("</header>", control + "</header>", StringComparison.Ordinal);
            }
            File.WriteAllText(path, html);
        }
        var indexPath = Path.Combine(root, "search-index.json");
        if (enabled)
            File.WriteAllText(indexPath, JsonSerializer.Serialize(entries, new JsonSerializerOptions { PropertyNamingPolicy = JsonNamingPolicy.CamelCase }));
        else if (File.Exists(indexPath))
            File.Delete(indexPath);
    }

    private static string PlainText(string html)
    {
        html = Regex.Replace(html, @"<(script|style)\b[^>]*>.*?</\1>", " ", RegexOptions.Singleline | RegexOptions.IgnoreCase);
        html = Regex.Replace(html, @"<[^>]*>", " ");
        return Regex.Replace(WebUtility.HtmlDecode(html), @"\s+", " ").Trim();
    }

    private sealed record SearchEntry(string Url, string Title, string Text);
}
