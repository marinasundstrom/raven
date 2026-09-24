using System.Text.Json;

using Markdig;
using Markdig.Syntax;
using Markdig.Syntax.Inlines;

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
        var apiPath = RelativeOutput(configuration.ApiPath);
        if (configuration.MemberListStyle is not ("compact" or "signatures"))
            throw new InvalidOperationException("memberListStyle must be compact or signatures.");
        if (IsWithin(output, root))
            throw new InvalidOperationException("Site output must not contain the configuration directory.");

        if (configuration.Toc is not null && configuration.Navigation.Count > 0)
            throw new InvalidOperationException("Use either 'toc' or 'navigation' to define the menu.");
        string ResolvePage(string source, string title)
        {
            var page = configuration.Pages.FirstOrDefault(page => Path.GetFullPath(page.Source, root) == source);
            if (page is null)
            {
                var relativeSource = Path.GetRelativePath(root, source).Replace('\\', '/');
                page = new SitePage(relativeSource, Path.ChangeExtension(relativeSource, ".html"), title);
                configuration.Pages.Add(page);
            }
            return page.Output ?? Path.ChangeExtension(page.Source, ".html");
        }
        var tocPath = configuration.Toc is not null ? Path.GetFullPath(configuration.Toc, root)
            : configuration.Navigation.Count == 0 && File.Exists(Path.Combine(root, "toc.yml")) ? Path.Combine(root, "toc.yml") : null;
        var menu = tocPath is null ? configuration.Navigation : DocumentationTableOfContents.Load(tocPath, root, ResolvePage);
        var sectionMenus = new Dictionary<string, IReadOnlyList<DocumentationNavigationItem>>(StringComparer.Ordinal);
        // Discover section menus before materializing pages; a toc may introduce more pages.
        for (var index = 0; index < configuration.Pages.Count; index++)
        {
            var directory = Path.GetDirectoryName(Path.GetFullPath(configuration.Pages[index].Source, root));
            while (directory is not null && directory != root && IsWithin(root, directory))
            {
                var sectionToc = Path.Combine(directory, "toc.yml");
                if (!sectionMenus.ContainsKey(directory) && File.Exists(sectionToc))
                    sectionMenus[directory] = DocumentationTableOfContents.Load(sectionToc, root, ResolvePage);
                directory = Path.GetDirectoryName(directory);
            }
        }
        IReadOnlyList<DocumentationNavigationItem> MenuForPage(string source)
        {
            var directory = Path.GetDirectoryName(source);
            while (directory is not null && directory != root && IsWithin(root, directory))
            {
                if (sectionMenus.TryGetValue(directory, out var section)) return section;
                directory = Path.GetDirectoryName(directory);
            }
            return menu;
        }

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
                page.Destination.StartsWith(apiPath + "/", StringComparison.OrdinalIgnoreCase) ||
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
                if (destination.StartsWith(apiPath + "/", StringComparison.OrdinalIgnoreCase) ||
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
                configuration.Footer ?? configuration.Name, configuration.MemberListStyle,
                configuration.Types, configuration.ExcludedMembers, configuration.Subtitle,
                configuration.Notice, configuration.ReleaseUrl, configuration.ReleaseLabel, configuration.ShowToc, configuration.Favicon);
            var template = new RavenDocSiteTemplate();
            template.WriteAssets(staging);
            IReadOnlyList<DocumentationNavigationItem> apiNavigation = [];
            if (apiInput is not null)
            {
                var apiOutput = Path.Combine(staging, apiPath);
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
                var metadata = PageFrontMatter.Parse(File.ReadAllText(page.Source));
                var markdown = MarkdownTemplate.Apply(metadata.Content, configuration.Values);
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
                var html = Path.GetExtension(page.Source).ToLowerInvariant() switch
                {
                    ".md" => Markdown.ToHtml(markdown, pipeline.Build()),
                    ".html" => markdown,
                    _ => throw new InvalidOperationException("Site content must be .md or .html.")
                };
                if (System.Text.RegularExpressions.Regex.IsMatch(html, @"<!doctype|<html\b|<head\b|<body\b", System.Text.RegularExpressions.RegexOptions.IgnoreCase))
                    throw new InvalidOperationException("HTML content must be a body fragment; RavenDoc supplies the page shell.");
                string Link(string path) => Path.GetRelativePath(currentDirectory, Path.Combine(staging, path)).Replace('\\', '/');
                var pageMenu = MenuForPage(page.Source);
                var navigation = DocumentationNavigation.Compose(pageMenu, apiNavigation, appendApi: ReferenceEquals(pageMenu, menu));
                var navigationRoot = configuration.ApiNavigationRoot?.TrimEnd('/');
                var showNavigation = metadata.Layout != "landing" && (navigationRoot is null ||
                    page.Destination.StartsWith(navigationRoot + "/", StringComparison.Ordinal));
                File.WriteAllText(destination, template.RenderPage(new RavenDocPageTemplateModel(
                    metadata.Title ?? page.Page.Title ?? Path.GetFileNameWithoutExtension(page.Source), "Documentation", configuration.Name,
                    Link("index.html"), Link("raven-theme.css"), Link("style.css"), Link("site.js"), html,
                    DocumentationNavigation.ResolveLinks(configuration.Links, staging, currentDirectory), showNavigation ? DocumentationNavigation.Render(navigation, staging, currentDirectory, destination) : "", configuration.Name,
                    DocumentationNavigation.Resolve(configuration.Logo, staging, currentDirectory),
                    DocumentationNavigation.Resolve(configuration.Stylesheet, staging, currentDirectory),
                    configuration.Footer ?? configuration.Name, configuration.Subtitle, configuration.Notice,
                    configuration.ReleaseUrl, configuration.ReleaseLabel, metadata.Layout, metadata.Toc ?? configuration.ShowToc,
                    DocumentationNavigation.Resolve(configuration.Favicon, staging, currentDirectory))));
            }
            if (apiInput is not null)
                File.WriteAllText(Path.Combine(staging, "xref-map.json"), JsonSerializer.Serialize(DocumentationGenerator.ExportXrefs(staging)));
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
        public string ApiPath { get; init; } = "api";
        public string MemberListStyle { get; init; } = "compact";
        public List<string>? Types { get; init; }
        public List<string>? ExcludedMembers { get; init; }
        public string? Subtitle { get; init; }
        public string? Notice { get; init; }
        public string? ReleaseUrl { get; init; }
        public string? ReleaseLabel { get; init; }
        public string? ApiNavigationRoot { get; init; }
        public bool ShowToc { get; init; } = true;
        public string Framework { get; init; } = "net10.0";
        public string? Logo { get; init; }
        public string? Favicon { get; init; }
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
