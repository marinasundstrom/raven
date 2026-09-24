using System.Text;

internal sealed class RavenDocSiteTemplate
{
    private const string StyleResourceName = "RavenDoc.Style.css";
    private const string ScriptResourceName = "RavenDoc.Site.js";
    private const string ThemeResourceName = "Raven.Theme.css";

    public void WriteAssets(string outputDirectory)
    {
        WriteResource(outputDirectory, ThemeResourceName, "raven-theme.css");
        WriteResource(outputDirectory, StyleResourceName, "style.css");
        WriteResource(outputDirectory, ScriptResourceName, "site.js");
        WriteResource(outputDirectory, "RavenDoc.Theme.js", "theme.js");
        WriteResource(outputDirectory, "Raven.Language.js", "raven-language.js");
        WriteResource(outputDirectory, "Raven.Highlight.css", "raven-highlight.css");
        WriteResource(outputDirectory, "RavenDoc.Highlight.js", "highlight-core.js");
        WriteResource(outputDirectory, "RavenDoc.Highlight.License", "highlight-LICENSE");
    }

    private static string ThemeIcon(string mode)
    {
        var shape = mode switch
        {
            "light" => "<circle cx=\"12\" cy=\"12\" r=\"4\"/><path d=\"M12 2v2m0 16v2M2 12h2m16 0h2M5 5l1.5 1.5m11 11L19 19M5 19l1.5-1.5m11-11L19 5\"/>",
            "dark" => "<path d=\"M20.5 14.4A8.5 8.5 0 0 1 9.6 3.5a8.5 8.5 0 1 0 10.9 10.9Z\"/>",
            _ => "<circle cx=\"12\" cy=\"12\" r=\"9\"/><path d=\"M12 3a9 9 0 0 0 0 18Z\" fill=\"currentColor\"/>"
        };
        return $"<svg class=\"theme-icon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.5\" stroke-linecap=\"round\">{shape}</svg>";
    }

    public string RenderPage(RavenDocPageTemplateModel page)
    {
        var title = $"{page.Title} · {page.ProjectName ?? page.AssemblyName}";
        var links = page.SiteLinks;
        var navigation = RenderMainNavigation(links);
        var logo = page.LogoHref is { } logoPath ? $"<img class=\"site-logo\" src=\"{Escape(logoPath)}\" alt=\"\" width=\"32\" height=\"32\" />" : "<span class=\"raven-brand-mark\">R</span>";
        var favicon = page.FaviconHref is { } icon ? $"<link rel=\"icon\" href=\"{Escape(icon)}\" />" : "";
        var customStyle = page.CustomStyleHref is { } style ? $"<link rel=\"stylesheet\" href=\"{Escape(style)}\" />" : "";
        var notice = string.IsNullOrWhiteSpace(page.Notice) ? "" : $"<div class=\"release-notice\" role=\"note\">{Escape(page.Notice)} {(page.ReleaseUrl is null ? "" : $"<a href=\"{Escape(page.ReleaseUrl)}\">{Escape(page.ReleaseLabel ?? "Published release")}</a>")}</div>";
        var analytics = "";
        if (page.GoogleAnalyticsId is { } measurementId)
        {
            if (!System.Text.RegularExpressions.Regex.IsMatch(measurementId, @"\AG-[A-Z0-9]+\z"))
                throw new InvalidOperationException("googleAnalyticsId must be a GA4 measurement ID (G-...).");
            analytics = $$"""
                <script async src="https://www.googletagmanager.com/gtag/js?id={{measurementId}}"></script>
                <script>
                  window.dataLayer = window.dataLayer || [];
                  function gtag() { dataLayer.push(arguments); }
                  gtag('js', new Date());
                  gtag('config', '{{measurementId}}');
                </script>
                """;
        }
        var showToc = page.ShowToc;
        var outline = showToc ? "<aside class=\"page-outline\" aria-label=\"On this page\"><div class=\"page-outline-card\"><strong>On this page</strong><nav id=\"page-outline-links\"></nav></div></aside>" : "";
        var layout = page.Layout;
        return $"""
        <!doctype html>
        <html lang="en">
        <head>
          <meta charset="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <title>{Escape(title)}</title>
          <script src="{Escape(page.ScriptHref[..^"site.js".Length] + "theme.js")}"></script>
          <link rel="stylesheet" href="{Escape(page.ThemeHref)}" />
          <link rel="stylesheet" href="{Escape(page.StyleHref)}" />
          {favicon}
          {customStyle}
          {analytics}
          <script type="module" src="{Escape(page.ScriptHref)}"></script>
        </head>
        <body class="{(page.NavigationHtml.Length > 0 ? "with-api-navigation " : "")}layout-{Escape(layout)}{(showToc ? "" : " without-outline")}">
          <a class="skip-link" href="#main">Skip to content</a>
          <header class="site-header">
            <a class="raven-brand" href="{Escape(page.HomeHref)}">
              {logo}
              <span class="raven-brand-copy">
                <strong>{Escape(page.ProjectName ?? page.AssemblyName)}</strong>
                <small>{Escape(page.Subtitle ?? "Raven API reference")}</small>
              </span>
            </a>
            {navigation}
            <details class="theme-menu">
              <summary aria-label="Color theme" aria-haspopup="menu" title="Color theme">
                <span class="theme-current theme-current--light">{ThemeIcon("light")}</span>
                <span class="theme-current theme-current--dark">{ThemeIcon("dark")}</span>
                <span class="theme-current theme-current--system">{ThemeIcon("system")}</span>
                <span class="theme-chevron" aria-hidden="true">▾</span>
              </summary>
              <div class="theme-options" role="menu" aria-label="Color theme">
                <button type="button" role="menuitemradio" data-theme-choice="light" aria-checked="false">{ThemeIcon("light")}<span>Light</span></button>
                <button type="button" role="menuitemradio" data-theme-choice="dark" aria-checked="false">{ThemeIcon("dark")}<span>Dark</span></button>
                <button type="button" role="menuitemradio" data-theme-choice="system" aria-checked="true">{ThemeIcon("system")}<span>Auto</span></button>
              </div>
            </details>
          </header>
          {notice}
          <div class="documentation-shell">
            {page.NavigationHtml}
            <main id="main" class="content-shell">
              <article class="api-content">
                {page.BodyHtml}
              </article>
            </main>
            {outline}
          </div>
          <footer>{Escape(page.Footer)} · Generated by RavenDoc</footer>
        </body>
        </html>
        """;
    }

    private static string RenderMainNavigation(IReadOnlyList<DocumentationSiteLink> links)
    {
        if (links.Count == 0) return "";
        string Items(IReadOnlyList<DocumentationSiteLink> items) => "<ul>" + string.Join("", items.Select(item =>
        {
            var label = Escape(item.Label);
            var link = string.IsNullOrEmpty(item.Url) ? $"<span>{label}</span>" : $"<a href=\"{Escape(item.Url)}\">{label}</a>";
            if (item.Children is not { Count: > 0 } children) return $"<li>{link}</li>";
            var overview = string.IsNullOrEmpty(item.Url) ? "" : $"<li><a href=\"{Escape(item.Url)}\">{label} overview</a></li>";
            return $"<li><details class=\"main-navigation-group\"><summary>{label}</summary><div>{(overview.Length == 0 ? "" : $"<ul>{overview}</ul>")}{Items(children)}</div></details></li>";
        })) + "</ul>";
        return $"<nav class=\"site-navigation\" aria-label=\"Main navigation\">{Items(links)}</nav>";
    }

    public string RenderHero(RavenDocSymbolKind kind, string kindLabel, string title, string? signature)
    {
        var signatureHtml = string.IsNullOrWhiteSpace(signature)
            ? string.Empty
            : $"""
              <pre class="api-signature"><code class="language-raven">{Escape(signature)}</code></pre>
              """;

        return $"""
        <header class="api-hero">
          <div class="api-kind">
            {RenderIcon(kind)}
            <span>{Escape(kindLabel)}</span>
          </div>
          <h1>{Escape(title)}</h1>
          {signatureHtml}
        </header>
        """;
    }

    public string RenderMemberSection(
        string title,
        IReadOnlyList<RavenDocMemberTemplateModel> members)
    {
        var style = DocumentationGenerator.MemberListStyle;
        var compact = style == "compact";
        var rows = new StringBuilder();
        foreach (var member in members)
        {
            var missingSummary = string.IsNullOrWhiteSpace(member.Summary);
            var summary = missingSummary
                ? member.Kind == RavenDocSymbolKind.Namespace ? "" : "<span class=\"member-summary member-summary--empty\">No summary available.</span>"
                : $"<span class=\"member-summary\">{Escape(member.Summary)}</span>";
            var label = compact ? member.Name : member.Signature;
            rows.AppendLine($"""
              <a class="member-card" href="{Escape(member.Href)}">
                {RenderIcon(member.Kind, member.IsStatic)}
                <span class="member-card-content">
                  <span class="{(style != "signatures" ? "member-name" : "member-signature")}">{Escape(label)}</span>
                  {summary}
                </span>
                <span class="member-arrow" aria-hidden="true">→</span>
              </a>
            """);
        }

        return $"""
        <section class="member-section" aria-labelledby="{GetHeadingId(title)}">
          <div class="section-heading">
            <h2 id="{GetHeadingId(title)}">{Escape(title)}</h2>
            <span>{members.Count} {Pluralize(members.Count, "item", "items")}</span>
          </div>
          <div class="member-list">
            {rows}
          </div>
        </section>
        """;
    }

    public string RenderCaseSection(
        IReadOnlyList<RavenDocCaseTemplateModel> cases)
    {
        var rows = new StringBuilder();
        foreach (var @case in cases)
        {
            var summary = string.IsNullOrWhiteSpace(@case.Summary)
                ? "<span class=\"member-summary member-summary--empty\">No summary available.</span>"
                : $"<span class=\"member-summary\">{Escape(@case.Summary)}</span>";

            rows.AppendLine($"""
              <div class="member-card member-card--static">
                {RenderIcon(RavenDocSymbolKind.Case)}
                <span class="member-card-content">
                  <span class="member-signature">{Escape(@case.Signature)}</span>
                  {summary}
                </span>
              </div>
            """);
        }

        return $"""
        <section class="member-section" aria-labelledby="cases">
          <div class="section-heading">
            <h2 id="cases">Cases</h2>
            <span>{cases.Count} {Pluralize(cases.Count, "case", "cases")}</span>
          </div>
          <div class="member-list">
            {rows}
          </div>
        </section>
        """;
    }

    public string RenderSignature(string signature)
        => $"""
           <pre class="api-signature api-signature--variant"><code class="language-raven">{Escape(signature)}</code></pre>
           """;

    internal static string RenderIcon(RavenDocSymbolKind kind, bool isStatic = false)
    {
        var (label, paths) = kind switch
        {
            RavenDocSymbolKind.Namespace => ("Namespace", """
                <path d="M4 5.5h6l1.5 2H20v11H4z" />
                <path d="M4 9h16" />
                """),
            RavenDocSymbolKind.Interface => ("Interface", """
                <path d="M5 4h14v16H5z" />
                <path d="M9 8h6M12 8v8M9 16h6" />
                """),
            RavenDocSymbolKind.Enum => ("Enum", """
                <path d="M5 4h14v16H5z" />
                <text x="12" y="16" text-anchor="middle" fill="currentColor" stroke="none" font-size="11" font-family="system-ui" font-weight="700">E</text>
                """),
            RavenDocSymbolKind.Union => ("Union", """
                <path d="M5 4h14v16H5z" />
                <text x="12" y="16" text-anchor="middle" fill="currentColor" stroke="none" font-size="11" font-family="system-ui" font-weight="700">U</text>
                """),
            RavenDocSymbolKind.Delegate => ("Delegate", """
                <path d="M5 4h14v16H5z" />
                <text x="12" y="16" text-anchor="middle" fill="currentColor" stroke="none" font-size="11" font-family="system-ui" font-weight="700">D</text>
                """),
            RavenDocSymbolKind.Struct => ("Struct", """
                <path d="M5 4h14v16H5z" />
                <text x="12" y="16" text-anchor="middle" fill="currentColor" stroke="none" font-size="11" font-family="system-ui" font-weight="700">S</text>
                """),
            RavenDocSymbolKind.Class => ("Class", """
                <path d="M5 4h14v16H5z" />
                <text x="12" y="16" text-anchor="middle" fill="currentColor" stroke="none" font-size="11" font-family="system-ui" font-weight="700">C</text>
                """),
            RavenDocSymbolKind.Type => ("Type", """
                <path d="M5 4h14v16H5z" />
                <path d="M9 8h6M12 8v8" />
                """),
            RavenDocSymbolKind.Function => ("Function", """
                <path d="M8 19c2-5 2-9 4-13 1-2 3-2 4 0" />
                <path d="M6 11h9" />
                """),
            RavenDocSymbolKind.Macro => ("Macro", """
                <path d="m12 3 1.4 5.6L19 10l-5.6 1.4L12 17l-1.4-5.6L5 10l5.6-1.4z" />
                <path d="m18 16 .6 2.4L21 19l-2.4.6L18 22l-.6-2.4L15 19l2.4-.6z" />
                """),
            RavenDocSymbolKind.Case => ("Case", """
                <circle cx="7" cy="7" r="2" />
                <circle cx="17" cy="17" r="2" />
                <path d="M9 7h2a6 6 0 0 1 6 6v2" />
                """),
            RavenDocSymbolKind.Property => ("Property", """
                <path d="M6 5h12v14H6z" />
                <path d="M9 9h6M9 13h4" />
                """),
            RavenDocSymbolKind.Field => ("Field", """
                <circle cx="12" cy="12" r="7" />
                <path d="M9 12h6" />
                """),
            RavenDocSymbolKind.Event => ("Event", """
                <path d="M12 3v5M12 16v5M3 12h5M16 12h5" />
                <circle cx="12" cy="12" r="4" />
                """),
            RavenDocSymbolKind.Operator => ("Operator", """
                <path d="M5 8h14M5 16h14M9 5v6M15 13v6" />
                """),
            _ => ("Member", """
                <circle cx="12" cy="12" r="7" />
                <path d="M12 9v6M9 12h6" />
                """)
        };

        return $"""
               <span class="symbol-icon symbol-icon--{kind.ToString().ToLowerInvariant()}" title="{(isStatic ? "Static member" : label)}" aria-hidden="true">
                 <svg viewBox="0 0 24 24" focusable="false">{paths}</svg>{(isStatic ? "<span class=\"symbol-static-marker\">S</span>" : "")}
               </span>{(isStatic ? "<span class=\"visually-hidden\">Static member: </span>" : "")}
               """;
    }

    private static string Pluralize(int count, string singular, string plural)
        => count == 1 ? singular : plural;

    private static string GetHeadingId(string title)
    {
        var builder = new StringBuilder(title.Length);
        var needsSeparator = false;
        foreach (var character in title)
        {
            if (char.IsLetterOrDigit(character))
            {
                if (needsSeparator && builder.Length > 0)
                    builder.Append('-');
                builder.Append(char.ToLowerInvariant(character));
                needsSeparator = false;
            }
            else
            {
                needsSeparator = true;
            }
        }

        return builder.Length == 0 ? "members" : builder.ToString();
    }

    internal static string Escape(string? value)
    {
        if (string.IsNullOrEmpty(value))
            return string.Empty;

        return value.Replace("&", "&amp;")
            .Replace("<", "&lt;")
            .Replace(">", "&gt;")
            .Replace("\"", "&quot;");
    }

    private static void WriteResource(
        string outputDirectory,
        string resourceName,
        string fileName)
    {
        using var stream = typeof(RavenDocSiteTemplate).Assembly
            .GetManifestResourceStream(resourceName)
            ?? throw new InvalidOperationException(
                $"The RavenDoc resource '{resourceName}' is missing.");
        using var reader = new StreamReader(stream);
        File.WriteAllText(Path.Combine(outputDirectory, fileName), reader.ReadToEnd());
    }
}

internal sealed record RavenDocPageTemplateModel(
    string Title,
    string Context,
    string AssemblyName,
    string HomeHref,
    string ThemeHref,
    string StyleHref,
    string ScriptHref,
    string BodyHtml,
    IReadOnlyList<DocumentationSiteLink> SiteLinks,
    string NavigationHtml = "",
    string? ProjectName = null,
    string? LogoHref = null,
    string? CustomStyleHref = null,
    string Footer = "Raven documentation",
    string? Subtitle = null,
    string? Notice = null,
    string? ReleaseUrl = null,
    string? ReleaseLabel = null,
    string Layout = "docs",
    bool ShowToc = true,
    string? FaviconHref = null,
    string? GoogleAnalyticsId = null);

internal sealed record RavenDocMemberTemplateModel(
    RavenDocSymbolKind Kind,
    string Signature,
    string Href,
    string Summary,
    string Name,
    bool IsStatic);

internal sealed record RavenDocCaseTemplateModel(
    string Signature,
    string Summary);

internal enum RavenDocSymbolKind
{
    Namespace,
    Type,
    Class,
    Interface,
    Enum,
    Union,
    Delegate,
    Struct,
    Function,
    Macro,
    Case,
    Property,
    Field,
    Event,
    Operator,
    Member
}
