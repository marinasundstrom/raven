using System.Text.Json;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class DocumentationSiteBuilderTests
{
    [Fact]
    public void MultipleLibrariesShareBrandingAndPreserveBothArticleXrefs()
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "first.rvn"), "namespace First { public class Widget { } }");
            File.WriteAllText(Path.Combine(root, "second.rvn"), "namespace Second { public class Gadget { } }");
            File.WriteAllText(Path.Combine(root, "index.md"), "# Home\n[Widget](xref:T:First.Widget) [Gadget](xref:T:Second.Gadget)");
            var config = Path.Combine(root, "site.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                name = "Shared website",
                search = true,
                apis = new[] {
                    new { input = "first.rvn", path = "libraries/first", title = "First library" },
                    new { input = "second.rvn", path = "libraries/second", title = "Second library" }
                },
                pages = new[] { new { source = "index.md" } }
            }));
            DocumentationSiteBuilder.Build(config);
            var home = File.ReadAllText(Path.Combine(root, "_site/index.html"));
            home.ShouldContain("href=\"libraries/first/First/Widget/index.html\"");
            home.ShouldContain("href=\"libraries/second/Second/Gadget/index.html\"");
            foreach (var path in new[] { "libraries/first/First/Widget", "libraries/second/Second/Gadget" })
            {
                var html = File.ReadAllText(Path.Combine(root, "_site", path, "index.html"));
                html.ShouldContain("Shared website");
                html.ShouldContain("First library");
                html.ShouldContain("Second library");
                html.ShouldContain("site-search-query");
            }
            var index = File.ReadAllText(Path.Combine(root, "_site/search-index.json"));
            index.ShouldContain("libraries/first/First/Widget/index.html");
            index.ShouldContain("libraries/second/Second/Gadget/index.html");
        });
    }

    [Theory]
    [InlineData("libraries/first")]
    [InlineData("libraries/first/nested")]
    public void OverlappingLibrariesPreservePublishedSite(string secondPath)
    {
        WithDirectory(root =>
        {
            Directory.CreateDirectory(Path.Combine(root, "_site"));
            File.WriteAllText(Path.Combine(root, "_site/index.html"), "Previous site");
            File.WriteAllText(Path.Combine(root, "index.md"), "# Home");
            var config = Path.Combine(root, "site.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                apis = new[] {
                    new { input = "first.rvn", path = "libraries/first" },
                    new { input = "second.rvn", path = secondPath }
                },
                pages = new[] { new { source = "index.md" } }
            }));
            Should.Throw<InvalidOperationException>(() => DocumentationSiteBuilder.Build(config))
                .Message.ShouldContain("overlap");
            File.ReadAllText(Path.Combine(root, "_site/index.html")).ShouldBe("Previous site");
        });
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CodeCopyIsOptInAndFinalizationDoesNotDuplicateScripts(bool enabled)
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "index.md"), "```raven\nlet value = 1\n```");
            var config = Path.Combine(root, "site.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                copyCode = enabled,
                pages = new[] { new { source = "index.md" } }
            }));
            DocumentationSiteBuilder.Build(config);
            DocumentationSiteBuilder.FinalizeSite(config);
            var html = File.ReadAllText(Path.Combine(root, "_site/index.html"));
            System.Text.RegularExpressions.Regex.Matches(html, "<!-- ravendoc-copy -->").Count.ShouldBe(enabled ? 1 : 0);
            html.ShouldContain("let value = 1");
        });
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SearchIndexesArticleTextAndFinalizesAdditionalApiPages(bool enabled)
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "index.md"), "# Welcome\n\nSearchable & useful.");
            var config = Path.Combine(root, "site.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                search = enabled,
                pages = new[] { new { source = "index.md" } }
            }));
            DocumentationSiteBuilder.Build(config);
            File.ReadAllText(Path.Combine(root, "_site/index.html")).ShouldContain("<title>Welcome · Documentation</title>");
            var output = Path.Combine(root, "_site");
            Directory.CreateDirectory(Path.Combine(output, "library/api"));
            File.WriteAllText(Path.Combine(output, "library/api/index.html"),
                "<title>Widget API</title><header></header><article><header>Distinctive contract</header> <script>secret</script></article>");
            DocumentationSiteBuilder.FinalizeSite(config);
            DocumentationSiteBuilder.FinalizeSite(config);
            var page = File.ReadAllText(Path.Combine(output, "library/api/index.html"));
            File.Exists(Path.Combine(output, "search-index.json")).ShouldBe(enabled);
            if (enabled)
            {
                page.ShouldContain("src=\"../../search.js\"");
                System.Text.RegularExpressions.Regex.Matches(page, "id=\"site-search-query\"").Count.ShouldBe(1);
                using var index = JsonDocument.Parse(File.ReadAllText(Path.Combine(output, "search-index.json")));
                index.RootElement.GetArrayLength().ShouldBe(2);
                var api = index.RootElement.EnumerateArray().Single(entry => entry.GetProperty("title").GetString() == "Widget API");
                api.GetProperty("text").GetString().ShouldBe("Distinctive contract");
            }
            else page.ShouldNotContain("site-search-query");
        });
    }

    [Fact]
    public void RawHtmlArticleLinksPreserveFragmentsAndCustomScriptDepth()
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "index.md"), "<a href=\"next.md#example\">Next</a>");
            File.WriteAllText(Path.Combine(root, "next.md"), "# Next");
            File.WriteAllText(Path.Combine(root, "site.json"), JsonSerializer.Serialize(new
            {
                script = "custom.js",
                pages = new[] { new { source = "index.md", output = "index.html" }, new { source = "next.md", output = "guide/next.html" } }
            }));
            DocumentationSiteBuilder.Build(Path.Combine(root, "site.json"));
            File.ReadAllText(Path.Combine(root, "_site/index.html")).ShouldContain("href=\"guide/next.html#example\"");
            File.ReadAllText(Path.Combine(root, "_site/guide/next.html")).ShouldContain("src=\"../custom.js\"");
        });
    }

    [Theory]
    [InlineData("")]
    [InlineData("G-ABC\n")]
    [InlineData("G-ABC';alert(1)//")]
    public void InvalidAnalyticsIdIsRejected(string id)
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "index.md"), "# Example");
            var config = Path.Combine(root, "site.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                googleAnalyticsId = id,
                pages = new[] { new { source = "index.md" } }
            }));
            Should.Throw<InvalidOperationException>(() => DocumentationSiteBuilder.Build(config))
                .Message.ShouldContain("googleAnalyticsId");
        });
    }

    [Fact]
    public void FrontMatterAndSectionTocKeepThreeNavigationLevelsIndependent()
    {
        WithDirectory(root =>
        {
            Directory.CreateDirectory(Path.Combine(root, "guide"));
            File.WriteAllText(Path.Combine(root, "index.html"), "---\ntitle: Welcome\nlayout: landing\ntoc: false\n---\n<section><h1>Hero</h1></section>");
            File.WriteAllText(Path.Combine(root, "guide/start.md"), "# Start\n\n## Details\n\nSection content.");
            File.WriteAllText(Path.Combine(root, "guide/next.md"), "---\ntoc: false\n---\n# Next");
            File.WriteAllText(Path.Combine(root, "guide/toc.yml"), "- name: Section start\n  href: start.md\n- name: Next step\n  href: next.md\n");
            var config = new
            {
                name = "Example",
                favicon = "brand/favicon.svg",
                notice = "Development documentation",
                links = new[] { new { label = "Learn", children = new[] { new { label = "Start", url = "learn/start.html" } } } },
                navigation = new[] { new { label = "Global side link", url = "index.html" } },
                pages = new[] { new { source = "index.html", output = "index.html" }, new { source = "guide/start.md", output = "learn/start.html" } }
            };
            File.WriteAllText(Path.Combine(root, "site.json"), JsonSerializer.Serialize(config));
            DocumentationSiteBuilder.Build(Path.Combine(root, "site.json"));
            var home = File.ReadAllText(Path.Combine(root, "_site/index.html"));
            home.ShouldContain("layout-landing without-outline");
            home.ShouldContain("rel=\"icon\" href=\"brand/favicon.svg\"");
            home.ShouldContain("main-navigation-group");
            home.ShouldContain("aria-label=\"Color theme\"");
            home.ShouldContain("<script src=\"theme.js\"");
            File.ReadAllText(Path.Combine(root, "_site/theme.js")).ShouldContain("ravendoc-theme");
            home.ShouldNotContain("id=\"api-browser\"");
            home.ShouldNotContain("aria-label=\"On this page\"");
            var guide = File.ReadAllText(Path.Combine(root, "_site/learn/start.html"));
            guide.ShouldContain("rel=\"icon\" href=\"../brand/favicon.svg\"");
            guide.ShouldContain("Section start");
            guide.ShouldContain("Next step");
            guide.ShouldNotContain("Global side link");
            guide.ShouldContain("aria-label=\"On this page\"");
            guide.ShouldContain("aria-controls=\"api-browser\"");
            var next = File.ReadAllText(Path.Combine(root, "_site/guide/next.html"));
            next.ShouldContain("aria-label=\"Documentation\"");
            next.ShouldNotContain("API Browser");
            next.ShouldNotContain("navigation-filter");
            next.ShouldNotContain("aria-label=\"On this page\"");
        });
    }

    [Theory]
    [InlineData("---\ntoc: maybe\n---\n# Bad")]
    [InlineData("---\nlayout: unknown\n---\n# Bad")]
    [InlineData("<html><body>Nested</body></html>")]
    public void InvalidPageControlsFailWithoutReplacingPublishedOutput(string content)
    {
        WithDirectory(root =>
        {
            Directory.CreateDirectory(Path.Combine(root, "_site"));
            File.WriteAllText(Path.Combine(root, "_site/index.html"), "Previous site");
            File.WriteAllText(Path.Combine(root, "index.html"), content);
            File.WriteAllText(Path.Combine(root, "site.json"), JsonSerializer.Serialize(new { pages = new[] { new { source = "index.html" } } }));
            Should.Throw<InvalidOperationException>(() => DocumentationSiteBuilder.Build(Path.Combine(root, "site.json")));
            File.ReadAllText(Path.Combine(root, "_site/index.html")).ShouldBe("Previous site");
        });
    }

    [Fact]
    public void SiteCombinesRelocatedGuidesApiLinksNavigationAndBranding()
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "library.rvn"), """
                namespace Example
                /// A useful widget.
                public class Widget { }
                """);
            Directory.CreateDirectory(Path.Combine(root, "guides"));
            Directory.CreateDirectory(Path.Combine(root, "images"));
            File.WriteAllText(Path.Combine(root, "index.md"), "# Welcome\n\n[Start](guides/start.md#usage)\n\n[Widget](xref:T:Example.Widget)");
            File.WriteAllText(Path.Combine(root, "guides/start.md"), "# Start\n\n## Usage\n\n[Home](../index.md)\n\n![Logo](../images/mark.svg)");
            File.WriteAllText(Path.Combine(root, "images/mark.svg"), "<svg xmlns=\"http://www.w3.org/2000/svg\" />");
            File.WriteAllText(Path.Combine(root, "custom.css"), ":root { --raven-accent: green; }");
            var configuration = new
            {
                name = "Example platform",
                namespaceNavigation = "flat",
                googleAnalyticsId = "G-TEST12345",
                api = "library.rvn",
                logo = "images/mark.svg",
                stylesheet = "custom.css",
                resources = new[] { "images", "custom.css" },
                pages = new[]
                {
                    new { source = "index.md", output = "index.html" },
                    new { source = "guides/start.md", output = "learn/intro.html" }
                },
                toc = "toc.yml"
            };
            File.WriteAllText(Path.Combine(root, "toc.yml"), """
                - name: Learn
                  items:
                    - name: Getting started
                      href: guides/start.md
                - name: Library reference
                  href: api/
                - name: Home
                  href: index.md
                """);
            var config = Path.Combine(root, "ravendoc.json");
            File.WriteAllText(config, JsonSerializer.Serialize(configuration));
            DocumentationSiteBuilder.Build(config);

            var home = File.ReadAllText(Path.Combine(root, "_site/index.html"));
            home.ShouldContain("href=\"learn/intro.html#usage\"");
            home.ShouldContain("href=\"api/Example/Widget/index.html\"");
            home.ShouldContain("Example platform");
            home.ShouldContain("href=\"custom.css\"");
            home.ShouldContain("Getting started");
            home.ShouldNotContain("api/System/index.html");
            home.ShouldContain("Library reference");
            home.IndexOf(">Library reference</span>", StringComparison.Ordinal)
                .ShouldBeGreaterThan(home.IndexOf(">Getting started</span>", StringComparison.Ordinal));
            home.IndexOf(">Home</span>", StringComparison.Ordinal)
                .ShouldBeGreaterThan(home.IndexOf(">Library reference</span>", StringComparison.Ordinal));
            var guide = File.ReadAllText(Path.Combine(root, "_site/learn/intro.html"));
            guide.ShouldContain("href=\"../index.html\"");
            guide.ShouldContain("src=\"../images/mark.svg\"");
            guide.ShouldContain("id=\"usage\"");
            var api = File.ReadAllText(Path.Combine(root, "_site/api/Example/Widget/index.html"));
            api.ShouldContain("href=\"../../../learn/intro.html\"");
            api.ShouldContain("src=\"../../../images/mark.svg\"");
            api.ShouldContain("class Widget");
            foreach (var page in new[] { home, guide, api })
            {
                page.ShouldContain("https://www.googletagmanager.com/gtag/js?id=G-TEST12345");
                page.ShouldContain("gtag('config', 'G-TEST12345')");
                page.Split("googletagmanager.com").Length.ShouldBe(2);
            }
            File.Exists(Path.Combine(root, "_site/custom.css")).ShouldBeTrue();
        });
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EmptyNamespaceVisibilityIsConfigurable(bool showEmptyNamespaces)
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "library.rvn"), """
                namespace Example.Child { public class Widget { } }
                namespace Example { internal class Hidden { } }
                """);
            File.WriteAllText(Path.Combine(root, "index.md"), "# Documentation");
            var config = Path.Combine(root, "ravendoc.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                api = "library.rvn",
                pages = new[] { new { source = "index.md", output = "index.html" } },
                showEmptyNamespaces
            }));
            DocumentationSiteBuilder.Build(config);
            var page = File.ReadAllText(Path.Combine(root, "_site/api/Example/Child/Widget/index.html"));
            var navigation = System.Text.RegularExpressions.Regex.Match(page,
                "<nav class=\"api-navigation-panel\"[^>]*>(.*?)</nav>",
                System.Text.RegularExpressions.RegexOptions.Singleline).Groups[1].Value;
            navigation.ShouldContain("Example.Child");
            navigation.Contains("<summary title=\"Example\">").ShouldBe(showEmptyNamespaces);
            File.Exists(Path.Combine(root, "_site/api/Example/index.html")).ShouldBeTrue();
        });
    }

    [Theory]
    [InlineData("missing")]
    [InlineData("unknown")]
    [InlineData("duplicate")]
    public void InvalidApiContentPreservesPublishedSite(string failure)
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "library.rvn"), "public class Widget { }");
            File.WriteAllText(Path.Combine(root, "index.md"), "# Welcome");
            var content = Path.Combine(root, "extras");
            Directory.CreateDirectory(content);
            var extra = Path.Combine(content, "widget.md");
            File.WriteAllText(extra, "---\nuid: T:Widget\n---\n## Usage\n\nAdded separately.");
            var config = Path.Combine(root, "ravendoc.json");
            File.WriteAllText(config, JsonSerializer.Serialize(new
            {
                api = "library.rvn",
                apiContent = "extras",
                pages = new[] { new { source = "index.md", output = "index.html" } }
            }));
            DocumentationSiteBuilder.Build(config);
            var page = Path.Combine(root, "_site/api/Widget/index.html");
            var published = File.ReadAllText(page);
            published.ShouldContain("Added separately.");
            if (failure == "duplicate") File.Copy(extra, Path.Combine(content, "duplicate.md"));
            else File.WriteAllText(extra, failure == "missing" ? "# No ID" : "---\nuid: T:Missing\n---\n# Unknown ID");
            Should.Throw<InvalidOperationException>(() => DocumentationSiteBuilder.Build(config));
            File.ReadAllText(page).ShouldBe(published);
        });
    }

    [Fact]
    public void AuthoredOnlySiteWorksAndFailedRebuildPreservesPublishedOutput()
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "index.md"), "# Original guide");
            var config = Path.Combine(root, "ravendoc.json");
            File.WriteAllText(config, """{"pages":[{"source":"index.md"}]}""");
            DocumentationSiteBuilder.Build(config);
            var output = Path.Combine(root, "_site/index.html");
            File.ReadAllText(output).ShouldContain("Original guide");
            File.WriteAllText(config, """{"pages":[{"source":"missing.md"}]}""");
            Should.Throw<FileNotFoundException>(() => DocumentationSiteBuilder.Build(config));
            File.ReadAllText(output).ShouldContain("Original guide");
        });
    }

    [Fact]
    public void NestedTocDiscoversPagesAndRejectsCircularIncludes()
    {
        WithDirectory(root =>
        {
            Directory.CreateDirectory(Path.Combine(root, "guide"));
            File.WriteAllText(Path.Combine(root, "index.md"), "# Home");
            File.WriteAllText(Path.Combine(root, "guide/start.md"), "# Start");
            File.WriteAllText(Path.Combine(root, "toc.yml"), """
                - name: Home
                  href: index.md
                - name: Guide
                  href: guide/toc.yml
                """);
            File.WriteAllText(Path.Combine(root, "guide/toc.yml"), """
                - name: Start here
                  href: start.md
                """);
            var config = Path.Combine(root, "ravendoc.json");
            File.WriteAllText(config, """{"toc":"toc.yml"}""");
            DocumentationSiteBuilder.Build(config);
            var output = Path.Combine(root, "_site/index.html");
            File.ReadAllText(output).ShouldContain("href=\"guide/start.html\"");
            File.Exists(Path.Combine(root, "_site/guide/start.html")).ShouldBeTrue();
            File.WriteAllText(Path.Combine(root, "guide/toc.yml"), """
                - name: Cycle
                  href: ../toc.yml
                """);
            Should.Throw<InvalidOperationException>(() => DocumentationSiteBuilder.Build(config));
            File.ReadAllText(output).ShouldContain("Start here");
        });
    }

    [Theory]
    [InlineData("""{"output":".","pages":[{"source":"index.md"}]}""")]
    [InlineData("""{"pages":[{"source":"index.md","output":"../index.html"}]}""")]
    [InlineData("""{"pages":[{"source":"index.md","output":"api/index.html"}]}""")]
    [InlineData("""{"pages":[{"source":"index.md"},{"source":"index.md"}]}""")]
    public void RejectsOverlappingAndEscapingOutputPaths(string configuration)
    {
        WithDirectory(root =>
        {
            File.WriteAllText(Path.Combine(root, "index.md"), "# Keep me");
            var config = Path.Combine(root, "ravendoc.json");
            File.WriteAllText(config, configuration);
            Should.Throw<InvalidOperationException>(() => DocumentationSiteBuilder.Build(config));
            File.ReadAllText(Path.Combine(root, "index.md")).ShouldBe("# Keep me");
        });
    }

    private static void WithDirectory(Action<string> action)
    {
        var root = Path.Combine(Path.GetTempPath(), "ravendoc-site-tests", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        try { action(root); }
        finally { Directory.Delete(root, recursive: true); }
    }
}
