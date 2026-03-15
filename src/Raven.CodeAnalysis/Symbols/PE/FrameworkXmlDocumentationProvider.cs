using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Reflection;
using System.Text;
using System.Text.Json;
using System.Xml.Linq;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal static class ExternalDocumentationProvider
{
    private static readonly ConcurrentDictionary<string, Lazy<ExternalDocumentationSet?>> s_files =
        new(StringComparer.OrdinalIgnoreCase);

    public static DocumentationComment? GetDocumentationComment(PESymbol symbol)
    {
        if (!TryGetMemberId(symbol, out var memberId))
            return null;

        if (!TryGetAssemblyLocation(symbol, out var assemblyLocation))
            return null;

        var docs = s_files.GetOrAdd(
            assemblyLocation,
            static path => new Lazy<ExternalDocumentationSet?>(() => ExternalDocumentationSet.TryLoad(path))).Value;

        if (docs is null)
            return null;

        return docs.GetDocumentationComment(memberId);
    }

    private static bool TryGetAssemblyLocation(PESymbol symbol, out string assemblyLocation)
    {
        assemblyLocation = string.Empty;

        try
        {
            if (symbol.ContainingAssembly is not PEAssemblySymbol peAssembly)
                return false;

            var assembly = peAssembly.GetAssemblyInfo();
            assemblyLocation = assembly.Location;
            return !string.IsNullOrWhiteSpace(assemblyLocation);
        }
        catch
        {
            return false;
        }
    }

    private static bool TryGetMemberId(PESymbol symbol, out string memberId)
    {
        memberId = symbol switch
        {
            PENamedTypeSymbol type => DocumentationCommentIdBuilder.GetTypeMemberId(type.GetTypeInfo().AsType()),
            PEMethodSymbol method => DocumentationCommentIdBuilder.GetMethodMemberId(method.ReflectionMethodBase),
            PEPropertySymbol property => DocumentationCommentIdBuilder.GetPropertyMemberId(property.GetPropertyInfo()),
            PEFieldSymbol field => DocumentationCommentIdBuilder.GetFieldMemberId(field.GetFieldInfo()),
            PEEventSymbol @event => DocumentationCommentIdBuilder.GetEventMemberId(@event.GetEventInfo()),
            _ => string.Empty
        };

        return memberId.Length > 0;
    }
}

internal sealed class ExternalDocumentationSet
{
    private readonly MarkdownDocumentationFile? _markdown;
    private readonly FrameworkXmlDocumentationFile? _xml;

    private ExternalDocumentationSet(MarkdownDocumentationFile? markdown, FrameworkXmlDocumentationFile? xml)
    {
        _markdown = markdown;
        _xml = xml;
    }

    public static ExternalDocumentationSet? TryLoad(string assemblyPath)
    {
        var markdown = MarkdownDocumentationFile.TryLoad(assemblyPath);
        var xml = FrameworkXmlDocumentationFile.TryLoad(assemblyPath);

        if (markdown is null && xml is null)
            return null;

        return new ExternalDocumentationSet(markdown, xml);
    }

    public DocumentationComment? GetDocumentationComment(string memberId)
    {
        if (_markdown?.TryGetMember(memberId, out var markdownComment) == true)
            return markdownComment;

        if (_xml?.TryGetMember(memberId, out var member) == true)
        {
            var xmlContent = XmlDocumentationMarkdownFormatter.GetXmlContent(member);
            if (!string.IsNullOrWhiteSpace(xmlContent))
            {
                return DocumentationComment.Create(
                    DocumentationFormat.Xml,
                    xmlContent,
                    rawText: member.ToString(SaveOptions.DisableFormatting));
            }
        }

        return null;
    }
}

internal sealed class MarkdownDocumentationFile
{
    private readonly ImmutableArray<string> _symbolsRootPaths;
    private readonly ConcurrentDictionary<string, DocumentationComment?> _cache = new(StringComparer.Ordinal);

    private MarkdownDocumentationFile(ImmutableArray<string> symbolsRootPaths)
    {
        _symbolsRootPaths = symbolsRootPaths;
    }

    public static MarkdownDocumentationFile? TryLoad(string assemblyPath)
    {
        var directory = Path.GetDirectoryName(assemblyPath);
        var assemblyName = Path.GetFileNameWithoutExtension(assemblyPath);
        if (string.IsNullOrWhiteSpace(directory) || string.IsNullOrWhiteSpace(assemblyName))
            return null;

        var docsRoot = Path.Combine(directory, $"{assemblyName}.docs");
        var manifestPath = Path.Combine(docsRoot, "manifest.json");
        if (!File.Exists(manifestPath))
            return null;

        try
        {
            using var stream = File.OpenRead(manifestPath);
            using var document = JsonDocument.Parse(stream);
            var root = document.RootElement;

            if (root.ValueKind != JsonValueKind.Object)
                return null;

            var formatVersion = root.TryGetProperty("formatVersion", out var formatVersionElement) &&
                                formatVersionElement.ValueKind == JsonValueKind.Number
                ? formatVersionElement.GetInt32()
                : 1;

            if (formatVersion != 1)
                return null;

            var symbolsPath = root.TryGetProperty("symbolsPath", out var symbolsPathElement) &&
                              symbolsPathElement.ValueKind == JsonValueKind.String
                ? symbolsPathElement.GetString()
                : "symbols";

            if (string.IsNullOrWhiteSpace(symbolsPath))
                return null;

            var localeRoots = ResolveLocaleRoots(root, docsRoot, symbolsPath!);
            if (localeRoots.IsDefaultOrEmpty)
                return null;

            return new MarkdownDocumentationFile(localeRoots);
        }
        catch
        {
            return null;
        }
    }

    public bool TryGetMember(string memberId, out DocumentationComment? comment)
    {
        comment = _cache.GetOrAdd(memberId, CreateDocumentationComment);
        return comment is not null;
    }

    private DocumentationComment? CreateDocumentationComment(string memberId)
    {
        foreach (var path in GetSymbolPaths(memberId))
        {
            if (!File.Exists(path))
                continue;

            try
            {
                var content = File.ReadAllText(path);
                if (string.IsNullOrWhiteSpace(content))
                    continue;

                return DocumentationComment.Create(
                    DocumentationFormat.Markdown,
                    content,
                    rawText: content);
            }
            catch
            {
                continue;
            }
        }

        return null;
    }

    private IEnumerable<string> GetSymbolPaths(string memberId)
    {
        if (memberId.Length < 3 || memberId[1] != ':')
            return [];

        var category = memberId[0].ToString();
        var encoded = DocumentationCommentIdBuilder.GetMarkdownPathHash(memberId) + ".md";
        return _symbolsRootPaths.Select(root => Path.Combine(root, category, encoded));
    }

    private static ImmutableArray<string> ResolveLocaleRoots(JsonElement root, string docsRoot, string symbolsPath)
    {
        var availableLocales = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        if (root.TryGetProperty("locales", out var localesElement) && localesElement.ValueKind == JsonValueKind.Array)
        {
            foreach (var locale in localesElement.EnumerateArray())
            {
                if (locale.ValueKind == JsonValueKind.String &&
                    !string.IsNullOrWhiteSpace(locale.GetString()))
                {
                    availableLocales.Add(locale.GetString()!);
                }
            }
        }

        if (availableLocales.Count == 0)
            return [Path.Combine(docsRoot, symbolsPath)];

        var defaultLocale = root.TryGetProperty("defaultLocale", out var defaultLocaleElement) &&
                            defaultLocaleElement.ValueKind == JsonValueKind.String &&
                            !string.IsNullOrWhiteSpace(defaultLocaleElement.GetString())
            ? defaultLocaleElement.GetString()!
            : "invariant";

        var orderedLocales = new List<string>();
        var currentCulture = System.Globalization.CultureInfo.CurrentUICulture;
        while (!string.IsNullOrWhiteSpace(currentCulture.Name))
        {
            orderedLocales.Add(currentCulture.Name);
            currentCulture = currentCulture.Parent;
        }

        orderedLocales.Add(defaultLocale);
        if (!string.Equals(defaultLocale, "invariant", StringComparison.OrdinalIgnoreCase))
            orderedLocales.Add("invariant");

        var builder = ImmutableArray.CreateBuilder<string>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var locale in orderedLocales)
        {
            if (!availableLocales.Contains(locale) || !seen.Add(locale))
                continue;

            builder.Add(Path.Combine(docsRoot, locale, symbolsPath));
        }

        return builder.ToImmutable();
    }
}

internal sealed class FrameworkXmlDocumentationFile
{
    private readonly Dictionary<string, XElement> _members;

    private FrameworkXmlDocumentationFile(Dictionary<string, XElement> members)
    {
        _members = members;
    }

    public static FrameworkXmlDocumentationFile? TryLoad(string assemblyPath)
    {
        var xmlPath = Path.ChangeExtension(assemblyPath, ".xml");
        if (string.IsNullOrWhiteSpace(xmlPath) || !File.Exists(xmlPath))
            return null;

        try
        {
            var document = XDocument.Load(xmlPath, LoadOptions.PreserveWhitespace);
            var members = document.Root?
                .Element("members")?
                .Elements("member")
                .Select(member => (Name: member.Attribute("name")?.Value, Element: member))
                .Where(static item => !string.IsNullOrWhiteSpace(item.Name))
                .ToDictionary(static item => item.Name!, static item => item.Element, StringComparer.Ordinal);

            if (members is null || members.Count == 0)
                return null;

            return new FrameworkXmlDocumentationFile(members);
        }
        catch
        {
            return null;
        }
    }

    public bool TryGetMember(string memberId, out XElement member)
        => _members.TryGetValue(memberId, out member!);
}

internal static class XmlDocumentationMarkdownFormatter
{
    public static string GetXmlContent(XElement member)
    {
        return string.Join(
            Environment.NewLine,
            member.Nodes().Select(static node => node.ToString(SaveOptions.DisableFormatting))).Trim();
    }

    public static string Format(XElement member)
    {
        var sections = new List<string>();

        AddIfNotEmpty(sections, RenderBlock(member.Element("summary")));

        var typeParameters = member.Elements("typeparam")
            .Select(param => (Name: param.Attribute("name")?.Value, Text: RenderBlock(param)))
            .Where(static item => !string.IsNullOrWhiteSpace(item.Name) && !string.IsNullOrWhiteSpace(item.Text))
            .Select(static item => $"- `{item.Name}`: {item.Text}")
            .ToArray();
        if (typeParameters.Length > 0)
            sections.Add("**Type Parameters**\n" + string.Join('\n', typeParameters));

        var parameters = member.Elements("param")
            .Select(param => (Name: param.Attribute("name")?.Value, Text: RenderBlock(param)))
            .Where(static item => !string.IsNullOrWhiteSpace(item.Name) && !string.IsNullOrWhiteSpace(item.Text))
            .Select(static item => $"- `{item.Name}`: {item.Text}")
            .ToArray();
        if (parameters.Length > 0)
            sections.Add("**Parameters**\n" + string.Join('\n', parameters));

        AddIfNotEmpty(sections, Prefix("**Returns**", RenderBlock(member.Element("returns"))));
        AddIfNotEmpty(sections, Prefix("**Value**", RenderBlock(member.Element("value"))));
        AddIfNotEmpty(sections, Prefix("**Remarks**", RenderBlock(member.Element("remarks"))));
        AddIfNotEmpty(sections, Prefix("**Example**", RenderBlock(member.Element("example"))));

        var exceptions = member.Elements("exception")
            .Select(ex =>
            {
                var cref = NormalizeCref(ex.Attribute("cref")?.Value);
                var text = RenderBlock(ex);
                return (cref, text);
            })
            .Where(static item => !string.IsNullOrWhiteSpace(item.cref) || !string.IsNullOrWhiteSpace(item.text))
            .Select(static item =>
                string.IsNullOrWhiteSpace(item.cref)
                    ? $"- {item.text}"
                    : $"- `{item.cref}`: {item.text}")
            .ToArray();
        if (exceptions.Length > 0)
            sections.Add("**Exceptions**\n" + string.Join('\n', exceptions));

        if (sections.Count > 0)
            return string.Join("\n\n", sections);

        // Fallback for unexpected XML payloads.
        return NormalizeWhitespace(RenderNodes(member.Nodes()));
    }

    private static string Prefix(string label, string value)
    {
        if (string.IsNullOrWhiteSpace(value))
            return string.Empty;

        return $"{label}\n{value}";
    }

    private static void AddIfNotEmpty(List<string> sections, string value)
    {
        if (!string.IsNullOrWhiteSpace(value))
            sections.Add(value);
    }

    private static string RenderBlock(XElement? element)
    {
        if (element is null)
            return string.Empty;

        return NormalizeWhitespace(RenderNodes(element.Nodes()));
    }

    private static string RenderNodes(IEnumerable<XNode> nodes)
    {
        var builder = new StringBuilder();
        foreach (var node in nodes)
            RenderNode(node, builder);

        return builder.ToString();
    }

    private static void RenderNode(XNode node, StringBuilder builder)
    {
        switch (node)
        {
            case XText text:
                builder.Append(text.Value);
                break;
            case XElement element:
                RenderElement(element, builder);
                break;
        }
    }

    private static void RenderElement(XElement element, StringBuilder builder)
    {
        switch (element.Name.LocalName)
        {
            case "see":
                var cref = NormalizeCref(element.Attribute("cref")?.Value);
                if (!string.IsNullOrWhiteSpace(cref))
                    builder.Append('`').Append(cref).Append('`');
                else if (element.Attribute("langword")?.Value is { Length: > 0 } langword)
                    builder.Append('`').Append(langword).Append('`');
                break;
            case "paramref":
            case "typeparamref":
                if (element.Attribute("name")?.Value is { Length: > 0 } name)
                    builder.Append('`').Append(name).Append('`');
                break;
            case "c":
                builder.Append('`').Append(NormalizeWhitespace(RenderNodes(element.Nodes()))).Append('`');
                break;
            case "code":
                var code = element.Value.Trim();
                if (code.Length > 0)
                    builder.Append("\n```text\n").Append(code).Append("\n```\n");
                break;
            case "para":
                builder.Append('\n').Append('\n');
                builder.Append(NormalizeWhitespace(RenderNodes(element.Nodes())));
                builder.Append('\n').Append('\n');
                break;
            case "br":
                builder.Append('\n');
                break;
            default:
                foreach (var child in element.Nodes())
                    RenderNode(child, builder);
                break;
        }
    }

    private static string NormalizeCref(string? cref)
    {
        if (string.IsNullOrWhiteSpace(cref))
            return string.Empty;

        if (cref.Length > 2 && cref[1] == ':')
            cref = cref.Substring(2);

        return cref.Replace("#ctor", "ctor", StringComparison.Ordinal);
    }

    private static string NormalizeWhitespace(string text)
    {
        if (string.IsNullOrWhiteSpace(text))
            return string.Empty;

        var normalizedNewlines = text.Replace("\r\n", "\n", StringComparison.Ordinal);
        var lines = normalizedNewlines.Split('\n')
            .Select(static line => line.Trim())
            .ToArray();

        // Collapse consecutive blank lines.
        var result = new List<string>(lines.Length);
        var lastBlank = false;
        foreach (var line in lines)
        {
            var isBlank = line.Length == 0;
            if (isBlank && lastBlank)
                continue;

            result.Add(line);
            lastBlank = isBlank;
        }

        return string.Join('\n', result).Trim();
    }
}
