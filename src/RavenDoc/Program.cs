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

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

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

    private static readonly Dictionary<ISymbol, SymbolDocInfo> DocInfoCache = new(ReferenceEqualityComparer<ISymbol>.Instance);

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
        string sourceCode = """
        namespace Samples

        import System.Console.*
        import System.Collections.Generic.List<>
        alias StringList = System.Collections.Generic.List<string>
        alias PrintLine = System.Console.WriteLine

        WriteLine("Hello"); val x = 2

        val user = Person(TheMeaningOfLife)
        user.AddRole("admin")

        val user2 = Person.WithName("John")
            .AddRole("admin");

        PrintLine(user2.Name)

        PrintLine(user2(2003));
        PrintLine(user2("test"));

        /// Just a base class
        public open class Base {}

        /// Initializes an instance of the Person class
        /// 
        /// ## Usage
        /// ```raven
        /// val person = Person(42)
        /// person.Test2()
        /// ```
        /// 
        public class Person : Base {
            /// Important constant
            public const TheMeaningOfLife: int = 42

            val species = "Homo sapiens"
            var age: int = 0
            var name: string
            var roles: StringList = []

            /// Constructor
            public init(age: int) {
                self.age = age
            }

            /// Named constructor
            public init WithName(name: string) {
                self.name = name
            }

            /// Regular method
            public AddRole(role: string) -> Person {
                roles.Add(role)
                self
            }

            /// Expression-bodies method
            public Test() -> int => 2

            /// Expression-bodies property
            public Test2: int => 2

            /// Computed property
            public Name: string {
                get {
                    name
                }
                set {
                    name = value
                }
            }

            /// Indexer: e.g., person[0]
            public self[index: int]: string {
                get => roles[index];
                set => roles[index] = value
            }

            /// Invocation operator: e.g., person(2025)
            public self(year: int) -> string {
                "Name: $name, Age in $year: ${year - (System.DateTime.Now.Year - age)}"
            }

            /// Another invocation operator
            public self(str: string) {

            }
        }
        """;

        SyntaxTree syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceCode);

        var compilation = Compilation.Create("test", [syntaxTree],
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

        var tree = compilation.SyntaxTrees.First();
        var sem = compilation.GetSemanticModel(tree);

        var globalNamespace = compilation.GetSourceGlobalNamespace();
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
        """;

        File.WriteAllText(Path.Combine(rootdir, "style.css"), css);
    }

    private static string WrapHtml(string currentDir, string pageLabelOrTitle, string assemblyName, string bodyHtml)
    {
        // Title format: "Member - Assembly" (or "TypeName - Assembly", etc.)
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
            segments.Push(cur.Name);
            cur = cur.ContainingType;
        }

        return Path.Combine(new[] { nsDir }.Concat(segments).ToArray());
    }

    private static string GetNamespaceIndexPath(INamespaceSymbol ns)
        => Path.Combine(GetNamespaceDir(ns), "index.html");

    private static string GetTypeIndexPath(ITypeSymbol type)
        => Path.Combine(GetTypeDir(type), "index.html");

    // One page per "member group" (e.g. Test, Name, self) not per overload.
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
    // Descriptor + member list rendering
    // ----------------------------

    private sealed class SymbolDocInfo
    {
        public string? RawMarkdown { get; init; }
        public string Summary { get; init; } = string.Empty;
    }

    private sealed class MemberRow
    {
        public required ISymbol Symbol { get; init; }
        public required string Signature { get; init; }     // markdown (link)
        public required string Summary { get; init; }       // plain-ish text for table cell
        public required bool IsContainer { get; init; }     // namespace/type
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

        // Normalize newlines and trim leading whitespace
        var text = markdown.Replace("\r\n", "\n").Trim();

        // Heuristic: first paragraph = first chunk separated by a blank line
        // Also ignore leading headings if present.
        var parts = text.Split(new[] { "\n\n" }, StringSplitOptions.RemoveEmptyEntries);

        foreach (var p in parts)
        {
            var para = p.Trim();
            if (para.Length == 0)
                continue;

            // Skip pure headings like "# Title" / "## Title"
            if (para.StartsWith("#"))
                continue;

            // Stop at an explicit section header starting early
            if (para.StartsWith("## "))
                continue;

            // Use first non-empty paragraph
            return ToTableCellText(para);
        }

        return string.Empty;
    }

    private static string ToTableCellText(string s)
    {
        // Keep it readable inside a Markdown table cell.
        // - Escape pipes
        // - Collapse newlines into spaces (or <br/> if you want multi-line)
        var t = s.Replace("|", "\\|")
                 .Replace("\r\n", "\n")
                 .Replace("\n", " ")
                 .Trim();

        // Avoid giant summaries: keep it short-ish (tune as you like)
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

            // Signature in the table is a link
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
            // If no summary, keep the cell empty rather than noise
            sb.AppendLine($"| {r.Signature} | {r.Summary} |");
        }

        sb.AppendLine();
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

        sb.AppendLine($"# {name}");
        if (typeSymbol.BaseType is not null)
        {
            var baseTypeSymbol = typeSymbol.BaseType;
            var target = GetTypeIndexPath(baseTypeSymbol);
            sb.AppendLine($"**Base type**: [{baseTypeSymbol.ToDisplayString(BaseTypeDisplayFormat)}]({RelLink(currentDir, target)})<br />");
        }
        if (typeSymbol.ContainingType is not null)
        {
            var containingType = typeSymbol.ContainingType!;
            var target = GetTypeIndexPath(containingType);
            sb.AppendLine($"**Containing type**: [{containingType.ToDisplayString(ContainingTypeDisplayFormat)}]({RelLink(currentDir, target)})<br />");
        }
        if (typeSymbol.ContainingNamespace is not null)
        {
            var containingNamespace = typeSymbol.ContainingNamespace!;
            var target = GetNamespaceIndexPath(containingNamespace);
            sb.AppendLine($"**Namespace**: [{containingNamespace.ToDisplayString(ContainingNamespaceDisplayFormat)}]({RelLink(currentDir, target)})<br />");
        }
        sb.AppendLine();

        if (!string.IsNullOrWhiteSpace(commentInfo.RawMarkdown))
            sb.Append(commentInfo.RawMarkdown);

        // Members
        var members = typeSymbol.GetMembers()
            .Where(GetMembersFilterPredicate)
            .Where(x => x is not IMethodSymbol ms || ms.AssociatedSymbol is null) // hide accessors
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .ToArray();

        sb.AppendLine();

        // Table for *everything* listed on the page (nested types + members + overloads)
        // Each overload row links to the grouped member page.
        AppendMemberTable(sb, "Members", currentDir, members);

        // Generate pages:
        // 1) nested types get their own folder/index
        foreach (var nestedType in members.OfType<ITypeSymbol>())
        {
            GenerateTypePage(compilation, nestedType);
        }

        // 2) group pages for non-type members
        var groups = members
            .Where(m => m is not ITypeSymbol)
            .GroupBy(GetMemberGroupKey);

        foreach (var g in groups)
        {
            GenerateMemberGroupPage(compilation, typeSymbol, g.Key, g.ToArray());
        }

        var contentHtml = Markdown.ToHtml(sb.ToString(), MarkdownPipeline);
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

        sb.AppendLine($"# {name}");
        {
            var target = GetTypeIndexPath(containingType);
            sb.AppendLine($"**Type**: [{containingType.ToDisplayString(ContainingTypeDisplayFormat)}]({RelLink(currentDir, target)})<br />");
        }
        if (containingType.ContainingNamespace is not null)
        {
            var ns = containingType.ContainingNamespace!;
            var target = GetNamespaceIndexPath(ns);
            sb.AppendLine($"**Namespace**: [{ns.ToDisplayString(ContainingNamespaceDisplayFormat)}]({RelLink(currentDir, target)})<br />");
        }
        sb.AppendLine();

        if (members.Count == 1)
        {
            var doc = GetOrCreateDocInfo(members[0]);
            if (!string.IsNullOrWhiteSpace(doc.RawMarkdown))
                sb.Append(doc.RawMarkdown);
            else
                sb.AppendLine("_No documentation available._");

            var htmlSingle = Markdown.ToHtml(sb.ToString(), MarkdownPipeline);
            var pageSingle = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", htmlSingle);
            File.WriteAllText(filePath, pageSingle);
            return;
        }

        // Only show overload section when there are overloads
        sb.AppendLine("## Overloads / Variants");
        sb.AppendLine();

        foreach (var member in members
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat)))
        {
            sb.AppendLine($"### {member.ToDisplayString(MemberDisplayFormat)}");
            sb.AppendLine();

            var doc = GetOrCreateDocInfo(member);
            if (!string.IsNullOrWhiteSpace(doc.RawMarkdown))
                sb.Append(doc.RawMarkdown);
            else
                sb.AppendLine("_No documentation available._");

            sb.AppendLine();
        }

        var overloadsHtml = Markdown.ToHtml(sb.ToString(), MarkdownPipeline);
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

        sb.AppendLine($"# {name}");
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

        // Optional: skip namespaces with no public content (your previous behavior)
        // (kept only for child namespaces)
        // For current namespace page we still render.

        AppendMemberTable(sb, "Members", currentDir, members);

        // Generate child pages (namespaces + types)
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

        // Rare: non-type member directly under namespace; if it happens, still generate a group page
        foreach (var m in members.Where(m => m is not INamespaceSymbol && m is not ITypeSymbol))
        {
            if (m.ContainingType is not null)
                GenerateMemberGroupPage(compilation, m.ContainingType, GetMemberGroupKey(m), new[] { m });
        }

        var contentHtml = Markdown.ToHtml(sb.ToString(), MarkdownPipeline);
        var pageHtml = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", contentHtml);
        File.WriteAllText(indexPath, pageHtml);
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
