using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Collections.Generic;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Markdig;
using Markdig.Extensions.AutoIdentifiers;
using Markdig.Syntax;
using Markdig.Syntax.Inlines;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;
using System.Diagnostics;

class Program
{
    private const string TargetFramework = "net9.0";
    private static readonly string rootdir = "_docs";

    private static readonly Func<ISymbol, bool> GetMembersFilterPredicate =
        x => x is INamespaceSymbol || x.DeclaredAccessibility == Accessibility.Public;

    // ----------------------------
    // Cached display formats
    // ----------------------------

    private static readonly SymbolDisplayFormat MemberDisplayFormat;
    private static readonly SymbolDisplayFormat BaseTypeDisplayFormat;
    private static readonly SymbolDisplayFormat ContainingNamespaceDisplayFormat;
    private static readonly SymbolDisplayFormat ContainingTypeDisplayFormat;

    // ----------------------------
    // Markdown pipeline + layout
    // ----------------------------

    public static MarkdownPipeline MarkdownPipeline { get; }

    // ----------------------------
    // Descriptor cache (docs/summary/signature/link)
    // ----------------------------

    private static readonly Dictionary<ISymbol, SymbolDocInfo> DocInfoCache
        = new(ReferenceEqualityComparer<ISymbol>.Instance);

    // ----------------------------
    // XRef index: "T:Foo.Bar" -> absolute output path
    // (we normalize overload IDs to member-group pages)
    // ----------------------------

    private static readonly Dictionary<string, string> XrefToTargetPath
        = new(StringComparer.Ordinal);

    static Program()
    {
        MarkdownPipeline = new MarkdownPipelineBuilder()
            .UseAdvancedExtensions() // tables, strikethrough, task lists, etc.
            .UseAutoIdentifiers(AutoIdentifierOptions.GitHub) // stable heading ids
            .Build();

        var miscOpt = SymbolDisplayFormat.FullyQualifiedFormat.MiscellaneousOptions
            | SymbolDisplayMiscellaneousOptions.ExpandAliases;

        var memberOpt = SymbolDisplayFormat.FullyQualifiedFormat.MemberOptions
            & ~SymbolDisplayMemberOptions.IncludeAccessibility;

        MemberDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithMiscellaneousOptions(miscOpt)
                .WithMemberOptions(memberOpt);

        BaseTypeDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly);

        ContainingTypeDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly);

        ContainingNamespaceDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithKindOptions(SymbolDisplayKindOptions.None);
    }

    static void Main()
    {
        try { Directory.Delete(rootdir, recursive: true); } catch { }
        try { Directory.CreateDirectory(rootdir); } catch { }

        WriteStyleSheet();
        Docs();
    }

    static void Docs()
    {
        var files = Directory.GetFiles("../Raven.Core", "*.rav");

        List<SyntaxTree> syntaxTrees = new List<SyntaxTree>();

        foreach (var file in files)
        {
            string sourceCode = File.ReadAllText(file);
            syntaxTrees.Add(ParseSyntaxTree(sourceCode));
        }

        var compilation = Compilation.Create("test", syntaxTrees.ToArray(),
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var version = TargetFrameworkResolver.ResolveVersion(TargetFramework);
        var refDir = TargetFrameworkResolver.GetDirectoryPath(version);

        var references = new[]
        {
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.dll")),
            MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Collections.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.Extensions.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.IO.FileSystem.dll")),
        };

        compilation = compilation.AddReferences(references);
        compilation.GetDiagnostics();

        var globalNamespace = compilation.GetSourceGlobalNamespace();

        // PASS 1: Build xref index (so forward references resolve)
        BuildXrefIndex(globalNamespace);

        // PASS 2: Generate pages
        ProcessSymbol(compilation, globalNamespace);
    }

    private static void ProcessSymbol(Compilation compilation, ISymbol symbol)
    {
        if (symbol is ITypeSymbol typeSymbol)
        {
            GenerateTypePage(compilation, typeSymbol);
        }
        else if (symbol is INamespaceSymbol namespaceSymbol)
        {
            GenerateNamespacePage(compilation, namespaceSymbol);
        }

        if (symbol is INamespaceOrTypeSymbol namespaceOrTypeSymbol)
        {
            foreach (var member in namespaceOrTypeSymbol.GetMembers().Where(GetMembersFilterPredicate))
            {
                ProcessSymbol(compilation, member);
            }
        }
    }

    // ----------------------------
    // Layout + styling
    // ----------------------------

    private static void WriteStyleSheet()
    {
        var css = """
        :root {
            --bg: #ffffff;
            --fg: #1f2937;
            --muted: #6b7280;
            --border: #e5e7eb;
            --code-bg: #f9fafb;
            --link: #2563eb;
        }

        * { box-sizing: border-box; }

        body {
            margin: 0;
            font-family: system-ui, -apple-system, BlinkMacSystemFont,
                         "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
            background: var(--bg);
            color: var(--fg);
            line-height: 1.6;
        }

        header {
            padding: 1rem 2rem;
            border-bottom: 1px solid var(--border);
            background: #fafafa;
            font-weight: 600;
        }

        main {
            max-width: 980px;
            padding: 2rem;
            margin: 0 auto;
        }

        h1, h2, h3, h4 {
            line-height: 1.25;
            margin-top: 2rem;
        }

        h1 { margin-top: 0; font-size: 2rem; }

        a {
            color: var(--link);
            text-decoration: none;
        }

        a:hover { text-decoration: underline; }

        pre, code {
            font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
            background: var(--code-bg);
        }

        pre {
            padding: 1rem;
            overflow-x: auto;
            border: 1px solid var(--border);
            border-radius: 6px;
        }

        code {
            padding: 0.2em 0.4em;
            border-radius: 4px;
        }

        table {
            border-collapse: collapse;
            margin: 1rem 0;
            width: 100%;
        }

        th, td {
            border: 1px solid var(--border);
            padding: 0.5rem 0.75rem;
            text-align: left;
            vertical-align: top;
        }

        th { background: #f3f4f6; }

        .muted { color: var(--muted); }

        /* Optional: broken xrefs show as plain text-ish */
        a.broken-xref { color: var(--muted); pointer-events: none; text-decoration: none; }
        """;

        File.WriteAllText(Path.Combine(rootdir, "style.css"), css);
    }

    private static string WrapHtml(string currentDir, string pageLabelOrTitle, string assemblyName, string bodyHtml)
    {
        var title = $"{pageLabelOrTitle} - {assemblyName}";
        var styleHref = RelLink(currentDir, Path.Combine(rootdir, "style.css"));

        return $"""
        <!doctype html>
        <html lang="en">
        <head>
          <meta charset="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <title>{HtmlEscape(title)}</title>
          <link rel="stylesheet" href="{styleHref}" />
        </head>
        <body>
          <header>{HtmlEscape(title)}</header>
          <main>
            {bodyHtml}
          </main>
        </body>
        </html>
        """;
    }

    private static string HtmlEscape(string s)
    {
        if (string.IsNullOrEmpty(s))
            return string.Empty;

        return s.Replace("&", "&amp;")
                .Replace("<", "&lt;")
                .Replace(">", "&gt;")
                .Replace("\"", "&quot;");
    }

    // ----------------------------
    // Path + link helpers
    // ----------------------------

    private static string RootDir => rootdir;

    private static string ToUrlPath(string path)
        => path.Replace(Path.DirectorySeparatorChar, '/');

    private static string GetNamespaceDir(INamespaceSymbol ns)
    {
        if (ns is null || ns.IsGlobalNamespace)
            return RootDir;

        var segments = new Stack<string>();
        var cur = ns;
        while (cur is not null && !cur.IsGlobalNamespace)
        {
            if (!string.IsNullOrWhiteSpace(cur.Name))
                segments.Push(cur.Name);
            cur = cur.ContainingNamespace;
        }

        return Path.Combine(new[] { RootDir }.Concat(segments).ToArray());
    }

    private static string GetTypeDir(ITypeSymbol type)
    {
        var nsDir = GetNamespaceDir(type.ContainingNamespace);

        var segments = new Stack<string>();
        var cur = type;

        while (cur is not null)
        {
            segments.Push(GetTypePathSegment(cur));
            cur = cur.ContainingType;
        }

        return Path.Combine(new[] { nsDir }.Concat(segments).ToArray());
    }

    private static string GetTypePathSegment(ITypeSymbol type)
    {
        if (type is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            type = named.OriginalDefinition;

        if (type is INamedTypeSymbol nts && nts.Arity > 0)
            return $"{nts.Name}`{nts.Arity}";

        return type.Name;
    }

    private static string GetNamespaceIndexPath(INamespaceSymbol ns)
        => Path.Combine(GetNamespaceDir(ns), "index.html");

    private static string GetTypeIndexPath(ITypeSymbol type)
        => Path.Combine(GetTypeDir(type), "index.html");

    private static string GetMemberGroupPath(ISymbol member)
    {
        var typeDir = GetTypeDir(member.ContainingType!);
        var groupKey = GetMemberGroupKey(member);
        var fileName = GetSafeFileName(groupKey) + ".html";
        return Path.Combine(typeDir, fileName);
    }

    private static void EnsureDirForFile(string filePath)
    {
        var dir = Path.GetDirectoryName(filePath);
        if (!string.IsNullOrWhiteSpace(dir))
            Directory.CreateDirectory(dir);
    }

    private static string RelLink(string fromDirectory, string toFileOrDirectory)
    {
        var rel = Path.GetRelativePath(fromDirectory, toFileOrDirectory);
        return ToUrlPath(rel);
    }

    // ----------------------------
    // Grouping + filename helpers
    // ----------------------------

    private static string GetMemberGroupKey(ISymbol member)
    {
        if (member is IMethodSymbol ms)
            return $"method:{ms.Name}";

        if (member is IPropertySymbol ps)
        {
            if (ps.Parameters is { Length: > 0 })
                return $"indexer:{ps.Name}";

            return $"property:{ps.Name}";
        }

        if (member is IFieldSymbol fs)
            return $"field:{fs.Name}";

        return $"{member.Kind}:{member.Name}";
    }

    private static string GetSafeFileName(string raw)
    {
        var sb = new StringBuilder(raw.Length);
        foreach (var ch in raw)
        {
            if (char.IsLetterOrDigit(ch))
                sb.Append(ch);
            else
                sb.Append('_');
        }

        if (sb.Length == 0)
            sb.Append("item");

        const int max = 140;
        if (sb.Length > max)
        {
            var hash = StableHash(raw);
            sb.Length = max;
            sb.Append("__");
            sb.Append(hash);
        }

        return sb.ToString();
    }

    private static string StableHash(string s)
    {
        unchecked
        {
            uint h = 2166136261;
            for (int i = 0; i < s.Length; i++)
                h = (h ^ s[i]) * 16777619;
            return h.ToString("x8");
        }
    }

    // ----------------------------
    // Docs cache + summary extraction
    // ----------------------------

    private sealed class SymbolDocInfo
    {
        public string? RawMarkdown { get; init; }
        public string Summary { get; init; } = string.Empty;
    }

    private sealed class MemberRow
    {
        public required ISymbol Symbol { get; init; }
        public required string Signature { get; init; }
        public required string Summary { get; init; }
        public required bool IsContainer { get; init; }
    }

    private static SymbolDocInfo GetOrCreateDocInfo(ISymbol symbol)
    {
        if (DocInfoCache.TryGetValue(symbol, out var cached))
            return cached;

        var comment = symbol.GetDocumentationComment();
        var raw = comment?.Content;

        var info = new SymbolDocInfo
        {
            RawMarkdown = raw,
            Summary = ExtractFirstParagraphSummary(raw)
        };

        DocInfoCache[symbol] = info;
        return info;
    }

    private static string ExtractFirstParagraphSummary(string? markdown)
    {
        if (string.IsNullOrWhiteSpace(markdown))
            return string.Empty;

        var text = markdown.Replace("\r\n", "\n").Trim();
        var parts = text.Split(new[] { "\n\n" }, StringSplitOptions.RemoveEmptyEntries);

        foreach (var p in parts)
        {
            var para = p.Trim();
            if (para.Length == 0)
                continue;

            if (para.StartsWith("#"))
                continue;

            if (para.StartsWith("## "))
                continue;

            return ToTableCellText(para);
        }

        return string.Empty;
    }

    private static string ToTableCellText(string s)
    {
        var t = s.Replace("|", "\\|")
                 .Replace("\r\n", "\n")
                 .Replace("\n", " ")
                 .Trim();

        const int max = 180;
        if (t.Length > max)
            t = t.Substring(0, max).TrimEnd() + "…";

        return t;
    }

    private static string GetTargetPathForLink(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol ns => GetNamespaceIndexPath(ns),
            ITypeSymbol ts => GetTypeIndexPath(ts),
            _ => GetMemberGroupPath(symbol),
        };
    }

    private static IReadOnlyList<MemberRow> BuildMemberRows(string currentDir, IEnumerable<ISymbol> members)
    {
        var rows = new List<MemberRow>();

        foreach (var m in members)
        {
            var path = GetTargetPathForLink(m);
            var href = RelLink(currentDir, path);

            var doc = GetOrCreateDocInfo(m);
            var summary = doc.Summary;

            var sigText = m.ToDisplayString(MemberDisplayFormat);
            var signature = $"[{sigText}]({href})";

            rows.Add(new MemberRow
            {
                Symbol = m,
                Signature = signature,
                Summary = string.IsNullOrWhiteSpace(summary) ? "" : summary,
                IsContainer = m is INamespaceSymbol || m is ITypeSymbol
            });
        }

        return rows;
    }

    private static void AppendMemberTable(StringBuilder sb, string title, string currentDir, IEnumerable<ISymbol> members)
    {
        var rows = BuildMemberRows(currentDir, members);

        sb.AppendLine($"## {title}");
        sb.AppendLine();
        sb.AppendLine("| Member | Summary |");
        sb.AppendLine("| --- | --- |");

        foreach (var r in rows)
        {
            sb.AppendLine($"| {EscapeName(r.Signature)} | {r.Summary} |");
        }

        sb.AppendLine();
    }

    // ----------------------------
    // XREF: indexing + Markdown rendering
    // ----------------------------

    private static void BuildXrefIndex(INamespaceSymbol globalNamespace)
    {
        void Visit(ISymbol s)
        {
            if (!GetMembersFilterPredicate(s))
                return;

            var id = GetXrefId(s);
            if (!string.IsNullOrWhiteSpace(id))
            {
                // Normalize overload IDs (M:/P: with params) to group IDs
                var normalized = NormalizeXrefIdForIndex(id);
                XrefToTargetPath[normalized] = GetTargetPathForLink(s);
            }

            if (s is ITypeSymbol ts && ts.Name.Contains("Result", StringComparison.OrdinalIgnoreCase))
            {
                Debug.WriteLine($"Type candidate: Name='{ts.Name}', Arity={(ts is INamedTypeSymbol n ? n.Arity : -1)}, " +
                                  $"Namespace='{(ts.ContainingNamespace?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) ?? "<null>")}', " +
                                  $"DocId='{id}'");
            }

            if (s is INamespaceOrTypeSymbol nts)
            {
                foreach (var m in nts.GetMembers())
                    Visit(m);
            }
        }

        Visit(globalNamespace);
    }

    private static string RenderMarkdownWithXrefs(string markdown, string currentDir)
    {
        // Build a per-page pipeline so we can capture currentDir in the callback.
        var builder = new MarkdownPipelineBuilder()
            .UseAdvancedExtensions()
            .UseAutoIdentifiers(AutoIdentifierOptions.GitHub);


        builder.DocumentProcessed += (doc) => RewriteXrefLinks(doc, currentDir);

        var pipeline = builder.Build();

        return Markdown.ToHtml(markdown, pipeline);
    }

    private static void RewriteXrefLinks(MarkdownDocument doc, string currentDir)
    {
        foreach (var link in doc.Descendants().OfType<LinkInline>())
        {
            if (link.IsImage || string.IsNullOrEmpty(link.Url))
                continue;

            const string prefix = "xref:";
            if (!link.Url.StartsWith(prefix, StringComparison.Ordinal))
                continue;

            var rawId = link.Url.Substring(prefix.Length); // e.g. "T:Raven.Core.Result`2"
            var normalized = NormalizeXrefIdIncoming(rawId);

            if (!XrefToTargetPath.TryGetValue(normalized, out var targetAbs))
            {
                link.Url = string.Empty;
                link.Title = $"Unresolved xref: {rawId}";

                // DEBUG: show a few suggestions (same suffix)
                var suggestions = XrefToTargetPath.Keys
                    .Where(k => k.EndsWith(normalized.Substring(normalized.IndexOf(':') + 1), StringComparison.Ordinal))
                    .Take(8)
                    .ToArray();

                if (suggestions.Length > 0)
                    Console.WriteLine($"Unresolved xref '{rawId}'. Did you mean:\n  - " + string.Join("\n  - ", suggestions));
                else
                    Console.WriteLine($"Unresolved xref '{rawId}'. No close matches. Total indexed: {XrefToTargetPath.Count}");

                continue;
            }

            link.Url = RelLink(currentDir, targetAbs);
        }
    }

    // Normalize stored IDs to match your member-group pages:
    // - strip "(...)" from M:/P: so any overload points to the same group page
    private static string NormalizeXrefIdForIndex(string id)
    {
        id = id.Trim();

        if (id.StartsWith("M:", StringComparison.Ordinal) || id.StartsWith("P:", StringComparison.Ordinal))
        {
            var paren = id.IndexOf('(');
            if (paren >= 0)
                return id.Substring(0, paren);
        }

        return id;
    }

    private static string NormalizeXrefIdIncoming(string id)
    {
        // Incoming comes from Markdown: xref:<id>
        // We normalize the same way as index keys.
        return NormalizeXrefIdForIndex(id);
    }

    // ----------------------------
    // XREF: symbol -> doc id
    // ----------------------------

    private static string GetXrefId(ISymbol s)
    {
        if (s is null)
            return string.Empty;

        return s switch
        {
            INamespaceSymbol ns => ns.IsGlobalNamespace ? "" : $"N:{GetNamespaceFullName(ns)}",
            ITypeSymbol ts => $"T:{GetTypeDocName(ts)}",
            IMethodSymbol ms => $"M:{GetMethodDocName(ms)}",
            IPropertySymbol ps => $"P:{GetPropertyDocName(ps)}",
            IFieldSymbol fs => $"F:{GetFieldDocName(fs)}",
            //IEventSymbol es => $"E:{GetEventDocName(es)}",
            _ => ""
        };
    }

    private static string GetNamespaceFullName(INamespaceSymbol ns)
    {
        var parts = new Stack<string>();
        var cur = ns;
        while (cur is not null && !cur.IsGlobalNamespace)
        {
            if (!string.IsNullOrEmpty(cur.Name))
                parts.Push(cur.Name);
            cur = cur.ContainingNamespace;
        }

        return string.Join(".", parts);
    }

    private static string GetTypeDocName(ITypeSymbol type)
    {
        // Namespace + containing types + type name with arity using `.
        // Nested types use '+' like XML doc IDs.
        //
        // Example: Raven.Core.Result`2
        // Example nested: Foo.Outer+Inner`1

        var sb = new StringBuilder();

        var ns = type.ContainingNamespace;
        if (ns is not null && !ns.IsGlobalNamespace)
        {
            sb.Append(GetNamespaceFullName(ns));
            sb.Append('.');
        }

        var chain = new Stack<ITypeSymbol>();
        for (var cur = type; cur is not null; cur = cur.ContainingType)
            chain.Push(cur);

        bool first = true;
        while (chain.Count > 0)
        {
            var t = chain.Pop();
            if (!first)
                sb.Append('+');
            first = false;

            if (t is INamedTypeSymbol nts && nts.IsGenericType)
            {
                var def = (nts.IsUnboundGenericType || nts.Equals(nts.OriginalDefinition))
                    ? nts
                    : nts.OriginalDefinition as INamedTypeSymbol;

                sb.Append(def.Name);
                sb.Append('`');
                sb.Append(def.Arity);
            }
            else
            {
                sb.Append(t.Name);
            }
        }

        return sb.ToString();
    }

    private static string GetMethodDocName(IMethodSymbol ms)
    {
        // Type.Method(params...)
        var sb = new StringBuilder();
        sb.Append(GetTypeDocName(ms.ContainingType!));
        sb.Append('.');
        sb.Append(ms.Name);

        if (ms.Parameters is { Length: > 0 })
        {
            sb.Append('(');
            for (int i = 0; i < ms.Parameters.Length; i++)
            {
                if (i > 0) sb.Append(',');
                sb.Append(GetParamTypeDocName(ms.Parameters[i].Type));
            }
            sb.Append(')');
        }

        return sb.ToString();
    }

    private static string GetPropertyDocName(IPropertySymbol ps)
    {
        var sb = new StringBuilder();
        sb.Append(GetTypeDocName(ps.ContainingType!));
        sb.Append('.');
        sb.Append(ps.Name);

        if (ps.Parameters is { Length: > 0 })
        {
            sb.Append('(');
            for (int i = 0; i < ps.Parameters.Length; i++)
            {
                if (i > 0) sb.Append(',');
                sb.Append(GetParamTypeDocName(ps.Parameters[i].Type));
            }
            sb.Append(')');
        }

        return sb.ToString();
    }

    private static string GetFieldDocName(IFieldSymbol fs)
        => $"{GetTypeDocName(fs.ContainingType!)}.{fs.Name}";

    //private static string GetEventDocName(IEventSymbol es)
    //    => $"{GetTypeDocName(es.ContainingType!)}.{es.Name}";

    private static string GetParamTypeDocName(ITypeSymbol t)
    {
        // Minimal starter: use type doc name where possible, else fallback to fully qualified.
        if (t is INamedTypeSymbol nts)
            return GetTypeDocName(nts);

        return t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
    }

    // ----------------------------
    // Page generators
    // ----------------------------

    private static void GenerateTypePage(Compilation compilation, ITypeSymbol typeSymbol)
    {
        var commentInfo = GetOrCreateDocInfo(typeSymbol);

        var indexPath = GetTypeIndexPath(typeSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        var sb = new StringBuilder();

        string name = typeSymbol.ToDisplayString(MemberDisplayFormat.WithKindOptions(SymbolDisplayKindOptions.None));

        sb.AppendLine($"# {EscapeName(name)}");
        if (typeSymbol.BaseType is not null)
        {
            var baseTypeSymbol = typeSymbol.BaseType;
            var target = GetTypeIndexPath(baseTypeSymbol);
            var memberName = EscapeName(baseTypeSymbol.ToDisplayString(BaseTypeDisplayFormat));
            sb.AppendLine($"**Base type**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }
        if (typeSymbol.ContainingType is not null)
        {
            var containingType = typeSymbol.ContainingType!;
            var target = GetTypeIndexPath(containingType);
            var memberName = EscapeName(containingType.ToDisplayString(ContainingTypeDisplayFormat));
            sb.AppendLine($"**Containing type**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }
        if (typeSymbol.ContainingNamespace is not null)
        {
            var containingNamespace = typeSymbol.ContainingNamespace!;
            var target = GetNamespaceIndexPath(containingNamespace);
            var memberName = EscapeName(containingNamespace.ToDisplayString(ContainingNamespaceDisplayFormat));
            sb.AppendLine($"**Namespace**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }
        sb.AppendLine();

        if (!string.IsNullOrWhiteSpace(commentInfo.RawMarkdown))
            sb.Append(commentInfo.RawMarkdown);

        var members = typeSymbol.GetMembers()
            .Where(GetMembersFilterPredicate)
            .Where(x => x is not IMethodSymbol ms || ms.AssociatedSymbol is null)
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .ToArray();

        sb.AppendLine();
        AppendMemberTable(sb, "Members", currentDir, members);

        foreach (var nestedType in members.OfType<ITypeSymbol>())
        {
            GenerateTypePage(compilation, nestedType);
        }

        var groups = members
            .Where(m => m is not ITypeSymbol)
            .GroupBy(GetMemberGroupKey);

        foreach (var g in groups)
        {
            GenerateMemberGroupPage(compilation, typeSymbol, g.Key, g.ToArray());
        }

        var contentHtml = RenderMarkdownWithXrefs(sb.ToString(), currentDir);
        var pageHtml = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }

    private static void GenerateMemberGroupPage(Compilation compilation, ITypeSymbol containingType, string groupKey, IReadOnlyList<ISymbol> members)
    {
        if (members.Count == 0)
            return;

        var filePath = GetMemberGroupPath(members[0]);
        EnsureDirForFile(filePath);
        var currentDir = Path.GetDirectoryName(filePath)!;

        var groupName = groupKey;
        var colon = groupName.IndexOf(':');
        if (colon >= 0 && colon + 1 < groupName.Length)
            groupName = groupName[(colon + 1)..];

        var sb = new StringBuilder();

        string name = members.Count == 1
            ? members[0].ToDisplayString(MemberDisplayFormat)
            : groupName;

        sb.AppendLine($"# {EscapeName(name)}");
        {
            var target = GetTypeIndexPath(containingType);
            var memberName = EscapeName(containingType.ToDisplayString(ContainingTypeDisplayFormat));
            sb.AppendLine($"**Type**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }
        if (containingType.ContainingNamespace is not null)
        {
            var ns = containingType.ContainingNamespace!;
            var target = GetNamespaceIndexPath(ns);
            var memberName = EscapeName(ns.ToDisplayString(ContainingNamespaceDisplayFormat));
            sb.AppendLine($"**Namespace**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }
        sb.AppendLine();

        if (members.Count == 1)
        {
            var doc = GetOrCreateDocInfo(members[0]);
            if (!string.IsNullOrWhiteSpace(doc.RawMarkdown))
                sb.Append(doc.RawMarkdown);
            else
                sb.AppendLine("_No documentation available._");

            var htmlSingle = RenderMarkdownWithXrefs(sb.ToString(), currentDir);
            var pageSingle = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", htmlSingle);
            File.WriteAllText(filePath, pageSingle);
            return;
        }

        sb.AppendLine("## Overloads / Variants");
        sb.AppendLine();

        foreach (var member in members
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat)))
        {
            var memberName = EscapeName(member.ToDisplayString(MemberDisplayFormat));

            sb.AppendLine($"### {memberName}");
            sb.AppendLine();

            var doc = GetOrCreateDocInfo(member);
            if (!string.IsNullOrWhiteSpace(doc.RawMarkdown))
                sb.Append(doc.RawMarkdown);
            else
                sb.AppendLine("_No documentation available._");

            sb.AppendLine();
        }

        var overloadsHtml = RenderMarkdownWithXrefs(sb.ToString(), currentDir);
        var pageHtml2 = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", overloadsHtml);
        File.WriteAllText(filePath, pageHtml2);
    }

    private static void GenerateNamespacePage(Compilation compilation, INamespaceSymbol namespaceSymbol)
    {
        var docInfo = GetOrCreateDocInfo(namespaceSymbol);

        var indexPath = GetNamespaceIndexPath(namespaceSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        var sb = new StringBuilder();

        string name = namespaceSymbol.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat.WithKindOptions(SymbolDisplayKindOptions.None));

        if (string.IsNullOrWhiteSpace(name))
            name = "Global namespace";

        sb.AppendLine($"# {EscapeName(name)}");
        sb.AppendLine();

        if (!string.IsNullOrWhiteSpace(docInfo.RawMarkdown))
            sb.Append(docInfo.RawMarkdown);

        sb.AppendLine();

        var members = namespaceSymbol.GetMembers()
            .Where(GetMembersFilterPredicate)
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .Where(x => x.Locations.Any(x => x.IsInSource) || x is INamespaceSymbol)
            .ToArray();

        AppendMemberTable(sb, "Members", currentDir, members);

        foreach (var ns2 in members.OfType<INamespaceSymbol>())
        {
            if (ns2.GetMembers().All(x => x.DeclaredAccessibility != Accessibility.Public))
                continue;

            GenerateNamespacePage(compilation, ns2);
        }

        foreach (var t2 in members.OfType<ITypeSymbol>())
        {
            GenerateTypePage(compilation, t2);
        }

        foreach (var m in members.Where(m => m is not INamespaceSymbol && m is not ITypeSymbol))
        {
            if (m.ContainingType is not null)
                GenerateMemberGroupPage(compilation, m.ContainingType, GetMemberGroupKey(m), new[] { m });
        }

        var contentHtml = RenderMarkdownWithXrefs(sb.ToString(), currentDir);
        var pageHtml = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }

    private static string EscapeName(string s)
    {
        return s.Replace("<", "&lt;").Replace(">", "&gt;");
    }

    // ----------------------------
    // Reference equality comparer (so we can use ISymbol keys safely)
    // ----------------------------

    private sealed class ReferenceEqualityComparer<T> : IEqualityComparer<T>
        where T : class
    {
        public static readonly ReferenceEqualityComparer<T> Instance = new();

        public bool Equals(T? x, T? y) => ReferenceEquals(x, y);
        public int GetHashCode(T obj) => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj);
    }
}
