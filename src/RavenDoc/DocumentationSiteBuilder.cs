using System.Text;
using System.Text.Json;

using Markdig;
using Markdig.Syntax;
using Markdig.Syntax.Inlines;

public sealed record DocumentationNavigationItem(
    string Label,
    string? Url = null,
    IReadOnlyList<DocumentationNavigationItem>? Children = null,
    bool Api = false);

internal static class DocumentationNavigation
{
    internal static IReadOnlyList<DocumentationNavigationItem> Compose(
        IReadOnlyList<DocumentationNavigationItem> items,
        IReadOnlyList<DocumentationNavigationItem> api)
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
        return placedApi ? expanded : expanded.Concat(api).ToArray();
    }

    internal static string? Resolve(string? url, string root, string currentDirectory)
    {
        if (url is null || url.StartsWith('/') || url.StartsWith('#') ||
            Uri.TryCreate(url, UriKind.Absolute, out _))
            return url;
        return Path.GetRelativePath(currentDirectory, Path.Combine(root, url)).Replace('\\', '/');
    }

    internal static string Render(IReadOnlyList<DocumentationNavigationItem> items, string root, string currentDirectory)
    {
        if (items.Count == 0)
            return string.Empty;
        var builder = new StringBuilder("<aside class=\"reference-navigation\"><nav aria-label=\"Documentation\"><label for=\"navigation-filter\">Filter navigation</label><input id=\"navigation-filter\" type=\"search\" placeholder=\"Find a page or type\" /><ul>");
        Append(items);
        builder.Append("</ul><p id=\"navigation-empty\" hidden>No matching pages.</p></nav></aside>");
        return builder.ToString();

        void Append(IReadOnlyList<DocumentationNavigationItem> entries)
        {
            foreach (var item in entries)
            {
                builder.Append("<li>");
                var label = RavenDocSiteTemplate.Escape(item.Label);
                if (item.Url is { } url)
                    builder.Append($"<a href=\"{RavenDocSiteTemplate.Escape(Resolve(url, root, currentDirectory))}\">{label}</a>");
                else
                    builder.Append($"<span>{label}</span>");
                if (item.Children is { Count: > 0 } children)
                {
                    builder.Append("<ul>");
                    Append(children);
                    builder.Append("</ul>");
                }
                builder.Append("</li>");
            }
        }
    }
}

/// <summary>Publishes authored Markdown and a Raven API reference using one site shell.</summary>
public static class DocumentationSiteBuilder
{
    public static void Build(string configurationPath)
    {
        configurationPath = Path.GetFullPath(configurationPath);
        var root = Path.GetDirectoryName(configurationPath)!;
        var configuration = JsonSerializer.Deserialize<SiteConfiguration>(File.ReadAllText(configurationPath),
            new JsonSerializerOptions { PropertyNameCaseInsensitive = true, UnmappedMemberHandling = System.Text.Json.Serialization.JsonUnmappedMemberHandling.Disallow })
            ?? throw new InvalidOperationException("The site configuration is empty.");
        var output = Path.GetFullPath(configuration.Output, root);
        if (IsWithin(output, root))
            throw new InvalidOperationException("Site output must not contain the configuration directory.");

        if (configuration.Toc is not null && configuration.Navigation.Count > 0)
            throw new InvalidOperationException("Use either 'toc' or 'navigation' to define the menu.");
        var menu = configuration.Toc is null
            ? configuration.Navigation
            : DocumentationTableOfContents.Load(Path.GetFullPath(configuration.Toc, root), root,
                (source, title) =>
                {
                    var page = configuration.Pages.FirstOrDefault(page =>
                        Path.GetFullPath(page.Source, root) == source);
                    if (page is null)
                    {
                        var relativeSource = Path.GetRelativePath(root, source).Replace('\\', '/');
                        page = new SitePage(relativeSource, Path.ChangeExtension(relativeSource, ".html"), title);
                        configuration.Pages.Add(page);
                    }
                    return page.Output ?? Path.ChangeExtension(page.Source, ".html");
                });

        var pages = configuration.Pages.Select(page => (
            Page: page,
            Source: Path.GetFullPath(page.Source, root),
            Destination: RelativeOutput(page.Output ?? Path.ChangeExtension(page.Source, ".html")))).ToArray();
        var destinations = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var page in pages)
        {
            if (!File.Exists(page.Source))
                throw new FileNotFoundException("Documentation page was not found.", page.Source);
            if (IsWithin(output, page.Source))
                throw new InvalidOperationException("Site output must not contain an input page.");
            if (!page.Destination.EndsWith(".html", StringComparison.OrdinalIgnoreCase) ||
                page.Destination.StartsWith("api/", StringComparison.OrdinalIgnoreCase) ||
                !destinations.Add(page.Destination))
                throw new InvalidOperationException($"Invalid or duplicate page output: {page.Destination}");
        }

        var assets = new List<(string Source, string Destination)>();
        foreach (var resource in configuration.Resources)
        {
            var source = Path.GetFullPath(resource, root);
            if (IsWithin(output, source) || (Directory.Exists(source) && IsWithin(source, output)))
                throw new InvalidOperationException("Site output must not contain an input resource.");
            foreach (var file in Directory.Exists(source)
                ? Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories)
                : new[] { source })
            {
                if (!File.Exists(file))
                    throw new FileNotFoundException("Site resource was not found.", file);
                var destination = RelativeOutput(Path.GetRelativePath(root, file));
                if (destination.StartsWith("api/", StringComparison.OrdinalIgnoreCase) ||
                    destination is "style.css" or "site.js" or "raven-theme.css" ||
                    !destinations.Add(destination))
                    throw new InvalidOperationException($"Conflicting resource output: {destination}");
                assets.Add((file, destination));
            }
        }

        var apiInput = configuration.Api is null ? null : Path.GetFullPath(configuration.Api, root);
        if (apiInput is not null && (IsWithin(output, apiInput) || IsWithin(apiInput, output)))
            throw new InvalidOperationException("API input and site output must be separate directories.");
        Directory.CreateDirectory(Path.GetDirectoryName(output)!);
        var staging = Path.Combine(Path.GetDirectoryName(output)!, $".ravendoc-{Guid.NewGuid():N}");
        Directory.CreateDirectory(staging);
        try
        {
            var options = new DocumentationSiteOptions(configuration.Links, configuration.Values, staging,
                configuration.Name, configuration.Logo, configuration.Stylesheet, menu,
                configuration.Footer ?? configuration.Name);
            var template = new RavenDocSiteTemplate();
            template.WriteAssets(staging);
            IReadOnlyList<DocumentationNavigationItem> apiNavigation = [];
            if (apiInput is not null)
            {
                var apiOutput = Path.Combine(staging, "api");
                if (apiInput.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
                    RavenDocCommand.GenerateFromAssembly(apiInput, apiOutput, configuration.Framework, options);
                else
                    RavenDocCommand.GenerateFromSource(apiInput, apiOutput, configuration.Framework, options,
                        configuration.References.Select(path => Path.GetFullPath(path, root)).ToArray());
                apiNavigation = DocumentationGenerator.GetApiNavigation();
            }

            var pageMap = pages.ToDictionary(page => page.Source,
                page => Path.Combine(staging, page.Destination), StringComparer.Ordinal);
            foreach (var page in pages)
            {
                var destination = pageMap[page.Source];
                var currentDirectory = Path.GetDirectoryName(destination)!;
                Directory.CreateDirectory(currentDirectory);
                var markdown = MarkdownTemplate.Apply(File.ReadAllText(page.Source), configuration.Values);
                var pipeline = new MarkdownPipelineBuilder().UseAdvancedExtensions();
                pipeline.DocumentProcessed += document =>
                {
                    foreach (var link in document.Descendants().OfType<LinkInline>())
                    {
                        if (link.Url is not { } url || url.StartsWith('/') || url.StartsWith('#') ||
                            Uri.TryCreate(url, UriKind.Absolute, out _))
                            continue;
                        var suffixStart = url.IndexOfAny(['#', '?']);
                        var path = suffixStart < 0 ? url : url[..suffixStart];
                        var suffix = suffixStart < 0 ? "" : url[suffixStart..];
                        var sourceTarget = Path.GetFullPath(Uri.UnescapeDataString(path), Path.GetDirectoryName(page.Source)!);
                        if (pageMap.TryGetValue(sourceTarget, out var target))
                            link.Url = Path.GetRelativePath(currentDirectory, target).Replace('\\', '/') + suffix;
                        else if (assets.FirstOrDefault(asset => asset.Source == sourceTarget) is var asset && asset.Source is not null)
                            link.Url = Path.GetRelativePath(currentDirectory, Path.Combine(staging, asset.Destination)).Replace('\\', '/') + suffix;
                    }
                };
                if (apiInput is not null)
                    pipeline.DocumentProcessed += document =>
                    {
                        foreach (var link in document.Descendants().OfType<LinkInline>())
                            if (link.Url?.StartsWith("xref:", StringComparison.Ordinal) == true)
                                link.Url = DocumentationGenerator.ResolveArticleXref(link.Url, currentDirectory);
                    };
                var html = Markdown.ToHtml(markdown, pipeline.Build());
                string Link(string path) => Path.GetRelativePath(currentDirectory, Path.Combine(staging, path)).Replace('\\', '/');
                var navigation = DocumentationNavigation.Compose(menu, apiNavigation);
                File.WriteAllText(destination, template.RenderPage(new RavenDocPageTemplateModel(
                    page.Page.Title ?? Path.GetFileNameWithoutExtension(page.Source), "Documentation", configuration.Name,
                    Link("index.html"), Link("raven-theme.css"), Link("style.css"), Link("site.js"), html,
                    configuration.Links.Select(link => link with
                    {
                        Url = DocumentationNavigation.Resolve(link.Url, staging, currentDirectory)!
                    }).ToArray(), DocumentationNavigation.Render(navigation, staging, currentDirectory), configuration.Name,
                    DocumentationNavigation.Resolve(configuration.Logo, staging, currentDirectory),
                    DocumentationNavigation.Resolve(configuration.Stylesheet, staging, currentDirectory),
                    configuration.Footer ?? configuration.Name)));
            }
            foreach (var asset in assets)
            {
                var destination = Path.Combine(staging, asset.Destination);
                Directory.CreateDirectory(Path.GetDirectoryName(destination)!);
                File.Copy(asset.Source, destination);
            }
            if (!File.Exists(Path.Combine(staging, "index.html")))
                throw new InvalidOperationException("The site must include a page with output 'index.html'.");
            if (Directory.Exists(output))
                Directory.Delete(output, recursive: true);
            Directory.Move(staging, output);
            Console.WriteLine($"RavenDoc wrote {output}");
        }
        finally
        {
            if (Directory.Exists(staging))
                Directory.Delete(staging, recursive: true);
        }
    }

    private static bool IsWithin(string directory, string path)
        => string.Equals(directory, path, StringComparison.OrdinalIgnoreCase) ||
           path.StartsWith(directory.TrimEnd(Path.DirectorySeparatorChar) + Path.DirectorySeparatorChar,
               StringComparison.OrdinalIgnoreCase);

    private static string RelativeOutput(string path)
    {
        path = path.Replace('\\', '/');
        if (Path.IsPathRooted(path) || path.Split('/').Any(part => part is ".." or "." or ""))
            throw new InvalidOperationException($"Output paths must be relative and stay within the site: {path}");
        return path;
    }

    private sealed class SiteConfiguration
    {
        public string Name { get; init; } = "Documentation";
        public string Output { get; init; } = "_site";
        public string? Toc { get; init; }
        public string? Api { get; init; }
        public string Framework { get; init; } = "net10.0";
        public string? Logo { get; init; }
        public string? Stylesheet { get; init; }
        public string? Footer { get; init; }
        public List<SitePage> Pages { get; init; } = [];
        public List<string> Resources { get; init; } = [];
        public List<string> References { get; init; } = [];
        public List<DocumentationNavigationItem> Navigation { get; init; } = [];
        public List<DocumentationSiteLink> Links { get; init; } = [];
        public Dictionary<string, string> Values { get; init; } = new(StringComparer.Ordinal);
    }

    private sealed record SitePage(string Source, string? Output = null, string? Title = null);
}
