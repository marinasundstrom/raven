using System.Text;

public sealed record DocumentationNavigationItem(
    string Label,
    string? Url = null,
    IReadOnlyList<DocumentationNavigationItem>? Children = null,
    bool Api = false,
    string? Kind = null);

internal static class DocumentationNavigation
{
    internal static IReadOnlyList<DocumentationNavigationItem> Compose(
        IReadOnlyList<DocumentationNavigationItem> items,
        IReadOnlyList<DocumentationNavigationItem> api, bool appendApi = true)
    {
        var placedApi = false;
        DocumentationNavigationItem Expand(DocumentationNavigationItem item)
        {
            if (item.Api)
            {
                placedApi = true;
                return item with { Url = api.FirstOrDefault()?.Url, Children = api.FirstOrDefault()?.Children };
            }
            return item with { Children = item.Children?.Select(Expand).ToArray() };
        }
        var expanded = items.Select(Expand).ToArray();
        return placedApi || !appendApi ? expanded : expanded.Concat(api).ToArray();
    }

    internal static string? Resolve(string? url, string root, string currentDirectory)
    {
        if (url is null || url.StartsWith('/') || url.StartsWith('#') ||
            Uri.TryCreate(url, UriKind.Absolute, out _))
            return url;
        return Path.GetRelativePath(currentDirectory, Path.Combine(root, url)).Replace('\\', '/');
    }

    internal static IReadOnlyList<DocumentationSiteLink> ResolveLinks(IReadOnlyList<DocumentationSiteLink> links, string root, string directory)
        => links.Select(link => link with
        {
            Url = string.IsNullOrEmpty(link.Url) ? "" : Resolve(link.Url, root, directory)!,
            Children = link.Children is null ? null : ResolveLinks(link.Children, root, directory)
        }).ToArray();

    internal static string Render(IReadOnlyList<DocumentationNavigationItem> items, string root, string currentDirectory, string? currentPage = null)
    {
        if (items.Count == 0) return "";
        var builder = new StringBuilder();
        var entries = items.SelectMany(item => item.Label == "API reference" && item.Children is { } children ? children : new[] { item }).ToArray();
        Append(entries);
        return $"""
            <button class="api-browser-toggle" type="button" aria-controls="api-browser" aria-expanded="false">Browse API</button>
            <dialog class="api-sidebar reference-navigation" id="api-browser" aria-labelledby="api-browser-heading" open>
              <div class="api-browser-header"><h2 id="api-browser-heading">API Browser</h2><button class="api-browser-close" type="button" aria-label="Close API Browser">×</button></div>
              <label class="visually-hidden" for="navigation-filter">Filter navigation</label>
              <input id="navigation-filter" type="search" placeholder="Find a page or type" />
              <nav class="api-navigation-panel" aria-label="API namespaces and types"><ul>{builder}</ul><p id="navigation-empty" hidden>No matching pages.</p></nav>
            </dialog>
            """;

        bool Current(DocumentationNavigationItem item) => item.Url is { } url &&
            !Uri.TryCreate(url, UriKind.Absolute, out _) &&
            (currentPage is null ? Path.GetDirectoryName(Path.GetFullPath(Path.Combine(root, url))) == Path.GetFullPath(currentDirectory)
                : Path.GetFullPath(Path.Combine(root, url)) == currentPage);
        bool ContainsCurrent(DocumentationNavigationItem item) => Current(item) || item.Children?.Any(ContainsCurrent) == true;
        void Append(IReadOnlyList<DocumentationNavigationItem> nodes)
        {
            foreach (var item in nodes)
            {
                var label = RavenDocSiteTemplate.Escape(item.Label);
                builder.Append("<li>");
                if (item.Children is { Count: > 0 } children)
                {
                    builder.Append($"<details class=\"api-namespace\"{(ContainsCurrent(item) ? " open" : "")}><summary title=\"{label}\">{Icon(item)}<span>{label}</span></summary><ul>");
                    if (item.Url is not null) AppendLink(item, item.Kind == "Namespace" ? "Namespace overview" : "Overview", true);
                    Append(children);
                    builder.Append("</ul></details>");
                }
                else AppendLink(item, item.Label, false);
                builder.Append("</li>");
            }
        }
        string Icon(DocumentationNavigationItem item) => item.Kind is null or "Namespace" ? "" : RavenDocSiteTemplate.RenderIcon(
                item.Kind switch { "Class" => RavenDocSymbolKind.Class, "Interface" => RavenDocSymbolKind.Interface, "Enum" => RavenDocSymbolKind.Enum, "Union" => RavenDocSymbolKind.Union, "Delegate" => RavenDocSymbolKind.Delegate, "Struct" => RavenDocSymbolKind.Struct, _ => RavenDocSymbolKind.Type });
        void AppendLink(DocumentationNavigationItem item, string text, bool wrap)
        {
            if (wrap) builder.Append("<li>");
            var label = RavenDocSiteTemplate.Escape(text);
            var icon = Icon(item);
            if (item.Url is { } url)
                builder.Append($"<a title=\"{RavenDocSiteTemplate.Escape(item.Label)}\" href=\"{RavenDocSiteTemplate.Escape(Resolve(url, root, currentDirectory))}\"{(Current(item) ? " aria-current=\"location\"" : "")}>{icon}<span>{label}</span></a>");
            else builder.Append($"<span>{label}</span>");
            if (wrap) builder.Append("</li>");
        }
    }

}
