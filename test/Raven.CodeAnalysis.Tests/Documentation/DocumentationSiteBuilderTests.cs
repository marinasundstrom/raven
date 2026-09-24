using System.Text.Json;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class DocumentationSiteBuilderTests
{
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
                notice = "Development documentation",
                links = new[] { new { label = "Learn", children = new[] { new { label = "Start", url = "learn/start.html" } } } },
                navigation = new[] { new { label = "Global side link", url = "index.html" } },
                pages = new[] { new { source = "index.html", output = "index.html" }, new { source = "guide/start.md", output = "learn/start.html" } }
            };
            File.WriteAllText(Path.Combine(root, "site.json"), JsonSerializer.Serialize(config));
            DocumentationSiteBuilder.Build(Path.Combine(root, "site.json"));
            var home = File.ReadAllText(Path.Combine(root, "_site/index.html"));
            home.ShouldContain("layout-landing without-outline");
            home.ShouldContain("main-navigation-group");
            home.ShouldNotContain("id=\"api-browser\"");
            home.ShouldNotContain("aria-label=\"On this page\"");
            var guide = File.ReadAllText(Path.Combine(root, "_site/learn/start.html"));
            guide.ShouldContain("Section start");
            guide.ShouldContain("Next step");
            guide.ShouldNotContain("Global side link");
            guide.ShouldContain("aria-label=\"On this page\"");
            guide.ShouldContain("aria-controls=\"api-browser\"");
            var next = File.ReadAllText(Path.Combine(root, "_site/guide/next.html"));
            next.ShouldContain("API Browser");
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
            File.Exists(Path.Combine(root, "_site/custom.css")).ShouldBeTrue();
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
