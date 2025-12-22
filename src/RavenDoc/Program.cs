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

        val user = Person(42)
        user.AddRole("admin")

        val user2 = Person.WithName("John")
            .AddRole("admin");

        PrintLine(user2.Name)

        PrintLine(user2(2003));
        PrintLine(user2("test"));

        open class Base {}

        /// Test
        /// | Month    | Savings |
        /// | -------- | ------- |
        /// | January  | $250    |
        /// | February | $80     |
        /// | March    | $420    |
        /// 
        class Person : Base {
            const TheMeaningOfLife: int = 42
            val species = "Homo sapiens"
            var age: int = 0
            var name: string
            var roles: StringList = []

            // Primary constructor
            public init(age: int) {
                self.age = age
            }

            // Named constructor
            public init WithName(name: string) {
                self.name = name
            }

            // Regular method
            public AddRole(role: string) -> Person {
                roles.Add(role)
                self
            }

            public Test() -> int => 2

            public Test2: int => 2

            // Computed property
            public Name: string {
                get {
                    name
                }
                set {
                    name = value
                }
            }

            // Indexer: e.g., person[0]
            public self[index: int]: string {
                get => roles[index];
                set => roles[index] = value
            }

            /// Invocation operator: e.g., person(2025)
            public self(year: int) -> string {
                "Name: $name, Age in $year: ${year - (System.DateTime.Now.Year - age)}"
            }

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
            foreach (var member in namespaceOrTypeSymbol.GetMembers())
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
            max-width: 920px;
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

    private static string WrapHtml(string currentDir, string pageLabel, string assemblyName, string bodyHtml)
    {
        // Title format: "Member - Assembly"
        var title = $"{pageLabel} - {assemblyName}";
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

        // minimal escaping (enough for titles/headers)
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
    // Page generators
    // ----------------------------

    private static void GenerateTypePage(Compilation compilation, ITypeSymbol typeSymbol)
    {
        var comment = typeSymbol.GetDocumentationComment();

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

        if (comment is not null)
            sb.Append(comment.Content);

        sb.AppendLine();
        sb.AppendLine("## Members");

        var members = typeSymbol.GetMembers()
            .Where(x => x is not IMethodSymbol ms || ms.AssociatedSymbol is null) // hide accessors
            .OrderBy(x => x.Name)
            .ToArray();

        foreach (var member in members)
        {
            if (member is ITypeSymbol nestedType)
            {
                var target = GetTypeIndexPath(nestedType);
                sb.AppendLine($"* [{nestedType.ToDisplayString(MemberDisplayFormat)}]({RelLink(currentDir, target)})");
                GenerateTypePage(compilation, nestedType);
            }
            else
            {
                var target = GetMemberGroupPath(member);
                sb.AppendLine($"* [{member.ToDisplayString(MemberDisplayFormat)}]({RelLink(currentDir, target)})");
            }
        }

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
            var comment = members[0].GetDocumentationComment();
            if (comment is not null)
                sb.Append(comment.Content);
            else
                sb.AppendLine("_No documentation available._");

            var contentHtml = Markdown.ToHtml(sb.ToString(), MarkdownPipeline);
            var pageHtml = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", contentHtml);
            File.WriteAllText(filePath, pageHtml);
            return;
        }

        sb.AppendLine("## Overloads / Variants");
        sb.AppendLine();

        foreach (var member in members.OrderBy(m => m.ToDisplayString(MemberDisplayFormat)))
        {
            sb.AppendLine($"### {member.ToDisplayString(MemberDisplayFormat)}");
            sb.AppendLine();

            var comment = member.GetDocumentationComment();
            if (comment is not null)
                sb.Append(comment.Content);
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
        var comment = namespaceSymbol.GetDocumentationComment();

        var indexPath = GetNamespaceIndexPath(namespaceSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        var sb = new StringBuilder();

        string name = namespaceSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithKindOptions(SymbolDisplayKindOptions.None));

        sb.AppendLine($"# {name}");
        sb.AppendLine();

        if (comment is not null)
            sb.Append(comment.Content);

        sb.AppendLine();
        sb.AppendLine("## Members");

        foreach (var member in namespaceSymbol.GetMembers()
            .OrderBy(x => x.Name)
            .Where(x => x.Locations.Any(x => x.IsInSource)))
        {
            if (member is INamespaceSymbol ns2)
            {
                var target = GetNamespaceIndexPath(ns2);
                sb.AppendLine($"* [{ns2.ToDisplayString(MemberDisplayFormat)}]({RelLink(currentDir, target)})");
                GenerateNamespacePage(compilation, ns2);
            }
            else if (member is ITypeSymbol t2)
            {
                var target = GetTypeIndexPath(t2);
                sb.AppendLine($"* [{t2.ToDisplayString(MemberDisplayFormat)}]({RelLink(currentDir, target)})");
                GenerateTypePage(compilation, t2);
            }
            else
            {
                var target = GetMemberGroupPath(member);
                sb.AppendLine($"* [{member.ToDisplayString(MemberDisplayFormat)}]({RelLink(currentDir, target)})");

                if (member.ContainingType is not null)
                    GenerateMemberGroupPage(compilation, member.ContainingType, GetMemberGroupKey(member), new[] { member });
            }
        }

        var contentHtml = Markdown.ToHtml(sb.ToString(), MarkdownPipeline);
        var pageHtml = WrapHtml(currentDir, name, compilation.AssemblyName ?? "Assembly", contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }
}
