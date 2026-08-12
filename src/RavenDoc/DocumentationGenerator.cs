using System.Text;

using Markdig;
using Markdig.Extensions.AutoIdentifiers;
using Markdig.Syntax;
using Markdig.Syntax.Inlines;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

public static class DocumentationGenerator
{
    private static string outputDir = "_docs";
    private static string documentedAssemblyName = "Raven";
    private static IAssemblySymbol? documentedAssembly;

    private const string ExtensionGroupingTypePrefix = "<>__RavenExtensionGrouping_For_";
    private const string ExtensionMarkerTypePrefix = "<>__RavenExtensionMarker_";
    private const string ExtensionMarkerMethodName = "<Extension>$";

    private static readonly Func<ISymbol, bool> GetMembersFilterPredicate =
        IsDocumentableSymbol;

    // ----------------------------
    // Cached display formats
    // ----------------------------

    private static readonly SymbolDisplayFormat MemberDisplayFormat;
    private static readonly SymbolDisplayFormat BaseTypeDisplayFormat;
    private static readonly SymbolDisplayFormat ContainingNamespaceDisplayFormat;
    private static readonly SymbolDisplayFormat ContainingTypeDisplayFormat;
    private static readonly RavenDocSiteTemplate SiteTemplate = new();
    private static readonly RavenDocContentTemplate ContentTemplate = new();

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

    private static readonly HashSet<string> ReportedBrokenXrefs
        = new(StringComparer.Ordinal);

    private static readonly Dictionary<string, List<ISymbol>> AdditionalNamespaceMembers
        = new(StringComparer.Ordinal);

    private static IReadOnlyList<DocumentationSiteLink> SiteLinks = [];
    private static string SiteRootDirectory = outputDir;
    private static IReadOnlyDictionary<string, string> TemplateValues =
        new Dictionary<string, string>();

    static DocumentationGenerator()
    {
        MarkdownPipeline = new MarkdownPipelineBuilder()
            .UseAdvancedExtensions() // tables, strikethrough, task lists, etc.
            .UseAutoIdentifiers(AutoIdentifierOptions.GitHub) // stable heading ids
            .Build();

        var miscOpt = SymbolDisplayFormat.FullyQualifiedFormat.MiscellaneousOptions
            | SymbolDisplayMiscellaneousOptions.ExpandAliases;

        MemberDisplayFormat =
            SymbolDisplayFormat.RavenSignatureFormat
                .WithGenericsOptions(
                    SymbolDisplayGenericsOptions.IncludeTypeParameters |
                    SymbolDisplayGenericsOptions.IncludeTypeConstraints)
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithMiscellaneousOptions(miscOpt)
                .WithMemberOptions(
                    SymbolDisplayFormat.RavenSignatureFormat.MemberOptions |
                    SymbolDisplayMemberOptions.IncludeAccessibility);

        BaseTypeDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithKindOptions(SymbolDisplayKindOptions.None);

        ContainingTypeDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithKindOptions(SymbolDisplayKindOptions.None);

        ContainingNamespaceDisplayFormat =
            SymbolDisplayFormat.FullyQualifiedFormat
                .WithTypeQualificationStyle(
                    SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
                .WithKindOptions(SymbolDisplayKindOptions.None);
    }

    public static void ProcessCompilation(
        Compilation compilation,
        string outputDir,
        DocumentationSiteOptions? siteOptions = null)
    {
        _ = compilation.GetDiagnostics();
        var additionalNamespaceMembers = compilation.SyntaxTrees
            .Concat(compilation.MacroSyntaxTrees)
            .Distinct()
            .SelectMany(tree =>
            {
                var semanticModel = compilation.GetSemanticModel(tree);
                var macros = tree.GetRoot()
                    .DescendantNodesAndSelf()
                    .OfType<MacroDeclarationSyntax>()
                    .Select(semanticModel.GetDeclaredSymbol)
                    .OfType<IMacroDeclarationSymbol>()
                    .Cast<ISymbol>();
                var namespaceFunctions = tree.GetRoot()
                    .DescendantNodesAndSelf()
                    .OfType<FunctionStatementSyntax>()
                    .Where(function =>
                        function.Parent is CompilationUnitSyntax or BaseNamespaceDeclarationSyntax)
                    .Select(semanticModel.GetDeclaredSymbol)
                    .OfType<IMethodSymbol>()
                    .Cast<ISymbol>();
                return macros.Concat(namespaceFunctions);
            })
            .Distinct(SymbolEqualityComparer.Default)
            .ToArray();
        Process(
            compilation,
            compilation.Assembly,
            compilation.GetSourceGlobalNamespace(),
            outputDir,
            additionalNamespaceMembers,
            siteOptions);
    }

    public static void ProcessAssembly(
        Compilation compilation,
        IAssemblySymbol assembly,
        string outputDir,
        DocumentationSiteOptions? siteOptions = null)
        => Process(
            compilation,
            assembly,
            assembly.GlobalNamespace,
            outputDir,
            additionalNamespaceMembers: [],
            siteOptions);

    private static void Process(
        Compilation compilation,
        IAssemblySymbol assembly,
        INamespaceSymbol globalNamespace,
        string outputDir,
        IReadOnlyList<ISymbol> additionalNamespaceMembers,
        DocumentationSiteOptions? siteOptions)
    {
        try { Directory.Delete(outputDir, recursive: true); } catch { }
        try { Directory.CreateDirectory(outputDir); } catch { }

        DocumentationGenerator.outputDir = outputDir;
        documentedAssembly = assembly;
        documentedAssemblyName = assembly.Name;
        DocInfoCache.Clear();
        XrefToTargetPath.Clear();
        ReportedBrokenXrefs.Clear();
        AdditionalNamespaceMembers.Clear();
        SiteLinks = siteOptions?.Links ?? [];
        SiteRootDirectory = Path.GetFullPath(siteOptions?.SiteRootDirectory ?? outputDir);
        TemplateValues = siteOptions?.TemplateValues ??
            new Dictionary<string, string>();
        foreach (var symbol in additionalNamespaceMembers)
        {
            var namespaceName = GetNamespaceFullName(symbol.ContainingNamespace);
            if (!AdditionalNamespaceMembers.TryGetValue(namespaceName, out var symbols))
            {
                symbols = [];
                AdditionalNamespaceMembers.Add(namespaceName, symbols);
            }

            symbols.Add(symbol);
        }

        SiteTemplate.WriteAssets(outputDir);

        // PASS 1: Build xref index (so forward references resolve)
        BuildXrefIndex(globalNamespace);
        foreach (var symbol in additionalNamespaceMembers)
            AddSymbolToXrefIndex(symbol);

        AssemblyNameToPath.Clear();

        foreach (var r in compilation.References)
        {
            // Roslyn-style: PortableExecutableReference has FilePath
            if (r is PortableExecutableReference per &&
                !string.IsNullOrWhiteSpace(per.FilePath))
            {
                var name = Path.GetFileNameWithoutExtension(per.FilePath);

                // Good-enough mapping; if you want perfect identity matching,
                // see the note below.
                AssemblyNameToPath[name] = per.FilePath;
            }
        }

        // PASS 2: Generate pages
        ProcessSymbol(compilation, globalNamespace);
        foreach (var namespaceRoot in additionalNamespaceMembers
            .Select(static symbol => GetOutermostNamespace(symbol.ContainingNamespace))
            .Where(static namespaceSymbol => namespaceSymbol is not null)
            .Distinct(SymbolEqualityComparer.Default)
            .Cast<INamespaceSymbol>())
        {
            ProcessSymbol(compilation, namespaceRoot);
        }
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
    }

    // ----------------------------
    // Layout + styling
    // ----------------------------

    private static string WrapHtml(string currentDir, string pageLabelOrTitle, string assemblyName, string bodyHtml)
    {
        var themeHref = RelLink(currentDir, Path.Combine(outputDir, "raven-theme.css"));
        var styleHref = RelLink(currentDir, Path.Combine(outputDir, "style.css"));
        var scriptHref = RelLink(currentDir, Path.Combine(outputDir, "site.js"));
        var homeHref = RelLink(currentDir, Path.Combine(SiteRootDirectory, "index.html"));

        return SiteTemplate.RenderPage(new RavenDocPageTemplateModel(
            pageLabelOrTitle,
            pageLabelOrTitle,
            assemblyName,
            homeHref,
            themeHref,
            styleHref,
            scriptHref,
            bodyHtml,
            SiteLinks));
    }

    private static string HtmlEscape(string s)
    {
        if (string.IsNullOrEmpty(s))
            return string.Empty;

        return RavenDocSiteTemplate.Escape(s);
    }

    // ----------------------------
    // Path + link helpers
    // ----------------------------

    private static string RootDir => outputDir;

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
        var typeDir = member.ContainingType is { } containingType &&
                      !IsAdditionalNamespaceMember(member)
            ? GetTypeDir(containingType)
            : GetNamespaceDir(member.ContainingNamespace);
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
    // GitHub source linking
    // ----------------------------

    // User asked for base repo URL:
    private const string GitHubRepoBaseUrl = "https://github.com/marinasundstrom/raven/";

    // You can change this if your default branch is different:
    private const string GitHubDefaultBranch = "main";

    private static string? TryFindRepoRoot(string startDir)
    {
        var dir = new DirectoryInfo(startDir);

        while (dir is not null)
        {
            // Most robust: detect .git folder
            if (Directory.Exists(Path.Combine(dir.FullName, ".git")))
                return dir.FullName;

            dir = dir.Parent;
        }

        return null;
    }

    private static string? GetSourceGitHubUrl(ISymbol symbol)
    {
        var src = symbol.Locations.FirstOrDefault(l => l is not null && l.IsInSource);
        var srcPath = src?.SourceTree?.FilePath;

        if (string.IsNullOrWhiteSpace(srcPath))
            return null;

        // Find repo root based on where the docs generator runs from.
        // (You can change this to use the directory of srcPath if you prefer.)
        var cwd = Directory.GetCurrentDirectory();
        var repoRoot = TryFindRepoRoot(cwd) ?? TryFindRepoRoot(Path.GetDirectoryName(srcPath) ?? cwd);
        if (string.IsNullOrWhiteSpace(repoRoot))
            return null;

        var rel = Path.GetRelativePath(repoRoot, srcPath);

        // Normalize + URL-encode each path segment
        var segments = rel
            .Split(new[] { Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar }, StringSplitOptions.RemoveEmptyEntries)
            .Select(Uri.EscapeDataString);

        var urlPath = string.Join("/", segments);

        // Optional: link to the first source location line
        var line = (src!.GetLineSpan().StartLinePosition.Line + 1); // 1-based
        var anchor = line > 0 ? $"#L{line}" : "";

        return $"{GitHubRepoBaseUrl}blob/{GitHubDefaultBranch}/{urlPath}{anchor}";
    }

    // ----------------------------
    // Grouping + filename helpers
    // ----------------------------

    private static string GetMemberGroupKey(ISymbol member)
    {
        if (member is IMethodSymbol ms)
        {
            if (IsOperatorLike(ms))
                return $"operator:{GetOperatorGroupName(ms)}";

            return $"method:{ms.Name}";
        }

        if (member is IPropertySymbol ps)
        {
            if (ps.Parameters is { Length: > 0 })
                return $"indexer:{ps.Name}";

            return $"property:{ps.Name}";
        }

        if (member is IFieldSymbol fs)
            return $"field:{fs.Name}";

        if (member is IMacroDeclarationSymbol macro)
            return $"macro:{macro.Name}";

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
    // Member grouping (by kind + logical order)
    // ----------------------------

    private enum MemberSectionKind
    {
        Namespaces,
        Types,
        Functions,
        Macros,
        Constants,
        Fields,
        Constructors,
        Properties,
        Indexers,
        Methods,
        Operators,
        Events,
        Other
    }

    private static readonly MemberSectionKind[] TypeMemberSectionOrder =
    {
    MemberSectionKind.Types,
    MemberSectionKind.Constants,
    MemberSectionKind.Fields,
    MemberSectionKind.Constructors,
    MemberSectionKind.Properties,
    MemberSectionKind.Indexers,
    MemberSectionKind.Methods,
    MemberSectionKind.Operators,
    MemberSectionKind.Events,
    MemberSectionKind.Other
};

    private static readonly MemberSectionKind[] NamespaceMemberSectionOrder =
    {
    MemberSectionKind.Namespaces,
    MemberSectionKind.Types,
    MemberSectionKind.Functions,
    MemberSectionKind.Macros,
    MemberSectionKind.Other
};

    private static string GetSectionTitle(MemberSectionKind kind) => kind switch
    {
        MemberSectionKind.Namespaces => "Namespaces",
        MemberSectionKind.Types => "Types",
        MemberSectionKind.Functions => "Functions",
        MemberSectionKind.Macros => "Macros",
        MemberSectionKind.Constants => "Constants",
        MemberSectionKind.Fields => "Fields",
        MemberSectionKind.Constructors => "Constructors",
        MemberSectionKind.Properties => "Properties",
        MemberSectionKind.Indexers => "Indexers",
        MemberSectionKind.Methods => "Methods",
        MemberSectionKind.Operators => "Operators",
        MemberSectionKind.Events => "Events",
        _ => "Members"
    };

    private static bool IsOperatorLike(IMethodSymbol ms)
    {
        // Raven: invocation operator and indexer-like call are named "self"
        if (IsInvocationOperator(ms))
            return true;

        if (ms.MethodKind is MethodKind.UserDefinedOperator or MethodKind.Conversion or MethodKind.BuiltinOperator)
            return true;

        // If you later model real operators explicitly in symbols, plug it in here.
        // For now, we can also treat known CLR-ish patterns as operators if you emit them:
        if (ms.Name is "op_Implicit" or "op_Explicit"
            or "op_Addition" or "op_Subtraction" or "op_Multiply" or "op_Division"
            or "op_Equality" or "op_Inequality" or "op_LessThan" or "op_GreaterThan"
            or "op_LessThanOrEqual" or "op_GreaterThanOrEqual"
            or "op_UnaryPlus" or "op_UnaryNegation" or "op_LogicalNot"
            or "op_BitwiseAnd" or "op_BitwiseOr" or "op_ExclusiveOr"
            or "op_LeftShift" or "op_RightShift"
            or "op_Modulus")
            return true;

        return false;
    }

    private static bool IsInvocationOperator(IMethodSymbol ms)
    {
        return ms.Name is "self" or "Invoke";
    }

    private static string GetOperatorGroupName(IMethodSymbol ms)
    {
        if (IsInvocationOperator(ms))
            return "self(...)";

        if (ms.MethodKind == MethodKind.Conversion)
            return GetConversionDisplayName(ms);

        return "operator " + GetOperatorToken(ms);
    }

    private static string GetConversionDisplayName(IMethodSymbol method)
    {
        return method.Name switch
        {
            "op_Implicit" => "implicit conversion",
            "op_Explicit" => "explicit conversion",
            _ => "conversion"
        };
    }

    private static string GetOperatorToken(IMethodSymbol method)
    {
        var name = method.Name;

        var checkedPrefix = false;
        const string checkedOpPrefix = "op_Checked";
        if (name.StartsWith(checkedOpPrefix, StringComparison.Ordinal))
        {
            checkedPrefix = true;
            name = "op_" + name.Substring(checkedOpPrefix.Length);
        }

        var token = name switch
        {
            "op_Addition" => "+",
            "op_Subtraction" => "-",
            "op_Multiply" => "*",
            "op_Division" => "/",
            "op_Modulus" => "%",

            "op_BitwiseAnd" => "&",
            "op_BitwiseOr" => "|",
            "op_ExclusiveOr" => "^",

            "op_LeftShift" => "<<",
            "op_RightShift" => ">>",

            "op_LogicalNot" => "!",
            "op_OnesComplement" => "~",
            "op_UnaryPlus" => "+",
            "op_UnaryNegation" => "-",

            "op_Increment" => "++",
            "op_Decrement" => "--",

            "op_Equality" => "==",
            "op_Inequality" => "!=",
            "op_LessThan" => "<",
            "op_LessThanOrEqual" => "<=",
            "op_GreaterThan" => ">",
            "op_GreaterThanOrEqual" => ">=",

            "op_True" => "true",
            "op_False" => "false",

            _ => name.StartsWith("op_", StringComparison.Ordinal) ? name.Substring(3) : name
        };

        return checkedPrefix ? $"checked {token}" : token;
    }

    private static MemberSectionKind GetMemberSectionForTypePage(ISymbol m)
    {
        return m switch
        {
            ITypeSymbol => MemberSectionKind.Types,

            IFieldSymbol fs when fs.IsConst => MemberSectionKind.Constants,
            IFieldSymbol => MemberSectionKind.Fields,

            IMethodSymbol ms when ms.AssociatedSymbol is not null => MemberSectionKind.Other, // accessors filtered elsewhere
            IMethodSymbol ms when ms.MethodKind == MethodKind.Constructor => MemberSectionKind.Constructors,
            IMethodSymbol ms when IsOperatorLike(ms) => MemberSectionKind.Operators,
            IMethodSymbol => MemberSectionKind.Methods,

            IPropertySymbol ps when ps.Parameters.Length > 0 => MemberSectionKind.Indexers,
            IPropertySymbol => MemberSectionKind.Properties,

            IEventSymbol => MemberSectionKind.Events,

            _ => MemberSectionKind.Other
        };
    }

    private static MemberSectionKind GetMemberSectionForNamespacePage(ISymbol m)
    {
        return m switch
        {
            INamespaceSymbol => MemberSectionKind.Namespaces,
            ITypeSymbol => MemberSectionKind.Types,
            IMethodSymbol => MemberSectionKind.Functions,
            IMacroDeclarationSymbol => MemberSectionKind.Macros,
            _ => MemberSectionKind.Other
        };
    }

    private static IReadOnlyList<string> RenderGroupedMemberSections(
        string currentDir,
        IEnumerable<ISymbol> members,
        bool isNamespacePage)
    {
        // Partition
        var grouped = members
            .GroupBy(m => isNamespacePage ? GetMemberSectionForNamespacePage(m) : GetMemberSectionForTypePage(m))
            .ToDictionary(g => g.Key, g => g.ToArray());

        var order = isNamespacePage ? NamespaceMemberSectionOrder : TypeMemberSectionOrder;

        var renderedSections = new List<string>();
        foreach (var section in order)
        {
            if (!grouped.TryGetValue(section, out var sectionMembers) || sectionMembers.Length == 0)
                continue;

            // Prefer a stable sort within each section:
            var ordered = sectionMembers
                .OrderBy(m => m.Name)
                .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
                .ToArray();

            renderedSections.Add(
                RenderMemberTable(GetSectionTitle(section), currentDir, ordered));
        }

        return renderedSections;
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
        public required string Href { get; init; }
        public required string Summary { get; init; }
        public required RavenDocSymbolKind Kind { get; init; }
    }

    private static SymbolDocInfo GetOrCreateDocInfo(ISymbol symbol)
    {
        if (DocInfoCache.TryGetValue(symbol, out var cached))
            return cached;

        DocumentationComment? comment;
        try
        {
            comment = symbol.GetDocumentationComment();
        }
        catch (BadImageFormatException exception)
        {
            Console.WriteLine(
                $"Skipping unreadable documentation metadata for '{symbol.Name}': {exception.Message}");
            comment = null;
        }

        var documentation = RavenDocumentationLoader.Load(comment);
        var markdown = MarkdownTemplate.Apply(
            BuildDocumentationMarkdown(documentation),
            TemplateValues);
        var info = new SymbolDocInfo
        {
            RawMarkdown = markdown,
            Summary = ExtractFirstParagraphSummary(markdown)
        };

        DocInfoCache[symbol] = info;
        return info;
    }

    private static string BuildDocumentationMarkdown(RavenDocumentation documentation)
    {
        var builder = new StringBuilder();
        var details = documentation.GetSection(DocumentationSectionKind.Details);
        var remarks = documentation.GetSection(DocumentationSectionKind.Remarks);

        AppendDocumentationSection(
            builder,
            documentation.GetSection(DocumentationSectionKind.Summary));
        AppendDocumentationSection(builder, details);
        AppendDocumentationAssociations(
            builder,
            "Type parameters",
            "Name",
            documentation.GetAssociations(DocumentationAssociationKind.TypeParameter));
        AppendDocumentationAssociations(
            builder,
            "Parameters",
            "Name",
            documentation.GetAssociations(DocumentationAssociationKind.Parameter));
        AppendNamedDocumentationSection(
            builder,
            "Returns",
            documentation.GetSection(DocumentationSectionKind.Result));
        AppendNamedDocumentationSection(
            builder,
            "Value",
            documentation.GetSection(DocumentationSectionKind.Value));
        AppendNamedDocumentationSection(
            builder,
            "Remarks",
            string.Equals(details?.Trim(), remarks?.Trim(), StringComparison.Ordinal)
                ? null
                : remarks);
        AppendNamedDocumentationSection(
            builder,
            "Example",
            documentation.GetSection(DocumentationSectionKind.Example));
        AppendDocumentationAssociations(
            builder,
            "Errors",
            "Error",
            documentation.GetAssociations(DocumentationAssociationKind.Error));
        AppendDocumentationAssociations(
            builder,
            "See also",
            "Reference",
            documentation.GetAssociations(DocumentationAssociationKind.RelatedLink));

        return builder.ToString().Trim();
    }

    private static void AppendDocumentationSection(StringBuilder builder, string? content)
    {
        if (string.IsNullOrWhiteSpace(content))
            return;

        if (builder.Length > 0)
            builder.AppendLine().AppendLine();
        builder.Append(content.Trim());
    }

    private static void AppendNamedDocumentationSection(
        StringBuilder builder,
        string title,
        string? content)
    {
        if (string.IsNullOrWhiteSpace(content))
            return;

        if (builder.Length > 0)
            builder.AppendLine().AppendLine();
        builder.AppendLine($"## {title}");
        builder.AppendLine();
        builder.Append(content.Trim());
    }

    private static void AppendDocumentationAssociations(
        StringBuilder builder,
        string title,
        string subjectHeader,
        IReadOnlyList<DocumentationAssociation> associations)
    {
        if (associations.Count == 0)
            return;

        if (builder.Length > 0)
            builder.AppendLine().AppendLine();
        builder.AppendLine($"## {title}");
        builder.AppendLine();
        builder.AppendLine($"| {subjectHeader} | Description |");
        builder.AppendLine("| --- | --- |");
        foreach (var association in associations)
        {
            var subject = association.Name ?? association.Reference ?? "—";
            builder.AppendLine(
                $"| `{ToTableCellText(subject)}` | {ToTableCellText(association.Content)} |");
        }
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
            IUnionCaseTypeSymbol @case => GetTypeIndexPath(@case.Union),
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

            var sigText = FormatSignature(m);
            rows.Add(new MemberRow
            {
                Symbol = m,
                Signature = sigText,
                Href = href,
                Summary = string.IsNullOrWhiteSpace(summary) ? "" : summary,
                Kind = GetTemplateSymbolKind(m)
            });
        }

        return rows;
    }

    private static bool CanRenderSymbol(ISymbol symbol)
    {
        if (IsCompilerGeneratedExtensionArtifact(symbol))
            return false;

        try
        {
            _ = symbol.ToDisplayString(MemberDisplayFormat);
            return true;
        }
        catch (BadImageFormatException exception)
        {
            Console.WriteLine(
                $"Skipping unreadable API symbol '{symbol.Name}': {exception.Message}");
            return false;
        }
    }

    private static string RenderMemberTable(
        string title,
        string currentDir,
        IEnumerable<ISymbol> members)
    {
        var rows = BuildMemberRows(currentDir, members);
        return SiteTemplate.RenderMemberSection(
            title,
            rows.Select(row => new RavenDocMemberTemplateModel(
                row.Kind,
                row.Signature,
                row.Href,
                row.Summary)).ToArray());
    }

    private static RavenDocSymbolKind GetTemplateSymbolKind(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol => RavenDocSymbolKind.Namespace,
            ITypeSymbol => RavenDocSymbolKind.Type,
            IMacroDeclarationSymbol => RavenDocSymbolKind.Macro,
            IMethodSymbol method when IsOperatorLike(method) => RavenDocSymbolKind.Operator,
            IMethodSymbol => RavenDocSymbolKind.Function,
            IPropertySymbol => RavenDocSymbolKind.Property,
            IFieldSymbol => RavenDocSymbolKind.Field,
            IEventSymbol => RavenDocSymbolKind.Event,
            _ => RavenDocSymbolKind.Member
        };
    }

    private static string GetSymbolKindLabel(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol => "Namespace",
            IMacroDeclarationSymbol => "Macro",
            IMethodSymbol method when IsOperatorLike(method) => "Operator",
            IMethodSymbol { MethodKind: MethodKind.Constructor } => "Constructor",
            IMethodSymbol when IsAdditionalNamespaceMember(symbol) => "Namespace function",
            IMethodSymbol => "Method",
            IPropertySymbol { IsIndexer: true } => "Indexer",
            IPropertySymbol => "Property",
            IFieldSymbol { IsConst: true } => "Constant",
            IFieldSymbol => "Field",
            IEventSymbol => "Event",
            ITypeSymbol type => type.TypeKind.ToString(),
            _ => "Member"
        };
    }

    // ----------------------------
    // Definition location helpers
    // ----------------------------

    // Optional: filled from references so we can show the DLL path for metadata types.
    private static readonly Dictionary<string, string> AssemblyNameToPath
        = new(StringComparer.OrdinalIgnoreCase);

    private static string GetDefinedInText(Compilation compilation, ISymbol symbol)
    {
        // Prefer source locations
        var src = symbol.Locations.FirstOrDefault(l => l is not null && l.IsInSource);
        var srcPath = src?.SourceTree?.FilePath;

        if (!string.IsNullOrWhiteSpace(srcPath))
            return $" {Path.GetFileName(srcPath)}";

        // Otherwise metadata (external assembly)
        var asm = symbol.ContainingAssembly;
        if (asm is not null)
        {
            var asmName = /* asm.Identity?.Name ?? */ asm.Name ?? "UnknownAssembly";

            if (AssemblyNameToPath.TryGetValue(asmName, out var dllPath) && !string.IsNullOrWhiteSpace(dllPath))
                return $"{asmName} ({Path.GetFileName(dllPath)})";

            return $"{asmName}";
        }

        return "Unknown location";
    }

    private static IReadOnlyList<string> GetSourceAndAssemblyLines(
        Compilation compilation,
        ISymbol symbol)
    {
        var lines = new List<string>();
        var asmFile = GetAssemblyFileNameForSymbol(compilation, symbol);
        lines.Add($"**Assembly**: {HtmlEscape(asmFile)}<br />");

        var sourceFileLine = GetSourceFileLine(symbol);
        if (sourceFileLine is not null)
            lines.Add(sourceFileLine);
        return lines;
    }

    private static string? GetSourceFileLine(ISymbol symbol)
    {
        var sourceFileName = GetSourceFileName(symbol);
        if (string.IsNullOrWhiteSpace(sourceFileName))
            return null;

        var githubUrl = GetSourceGitHubUrl(symbol);

        return !string.IsNullOrWhiteSpace(githubUrl)
            ? $"**Source file**: [{EscapeName(sourceFileName)}]({githubUrl})<br />"
            : $"**Source file**: {HtmlEscape(sourceFileName)}<br />";
    }

    private static string GetOutputAssemblyFileName(Compilation compilation)
    {
        var name = documentedAssemblyName;
        if (string.IsNullOrWhiteSpace(name))
            name = "UnknownAssembly";

        // If you have an API equivalent to Roslyn's OutputKind, use it.
        var ext =
            compilation.Options?.OutputKind == OutputKind.ConsoleApplication ||
            compilation.Options?.OutputKind == OutputKind.WindowsApplication
                ? ".exe"
                : ".dll";

        return name + ext;
    }

    private static string? GetSourceFileName(ISymbol symbol)
    {
        var src = symbol.Locations.FirstOrDefault(l => l is not null && l.IsInSource);
        var srcPath = src?.SourceTree?.FilePath;
        return string.IsNullOrWhiteSpace(srcPath) ? null : Path.GetFileName(srcPath);
    }

    private static string GetAssemblyFileNameForSymbol(Compilation compilation, ISymbol symbol)
    {
        // Source symbol: use compilation output
        if (symbol.Locations.Any(l => l is not null && l.IsInSource))
            return GetOutputAssemblyFileName(compilation);

        // Metadata symbol: use containing assembly (+ mapped dll file name if we have it)
        var asm = symbol.ContainingAssembly;
        if (asm is not null)
        {
            var asmName = asm.Name ?? "UnknownAssembly";

            if (AssemblyNameToPath.TryGetValue(asmName, out var dllPath) && !string.IsNullOrWhiteSpace(dllPath))
                return Path.GetFileName(dllPath); // e.g. System.Runtime.dll

            // Fallback: best guess
            return asmName + ".dll";
        }

        return "UnknownAssembly";
    }

    // ----------------------------
    // XREF: indexing + Markdown rendering
    // ----------------------------

    private static void BuildXrefIndex(INamespaceSymbol globalNamespace)
    {
        static void Visit(ISymbol s)
        {
            if (!GetMembersFilterPredicate(s))
                return;

            AddSymbolToXrefIndex(s);

            if (s is IUnionSymbol union)
            {
                foreach (var @case in union.DeclaredCaseTypes)
                    AddSymbolToXrefIndex(@case);
            }

            if (s is INamespaceOrTypeSymbol nts)
            {
                foreach (var m in nts.GetMembers())
                    Visit(m);
            }
        }

        Visit(globalNamespace);
    }

    private static void AddSymbolToXrefIndex(ISymbol symbol)
    {
        var id = GetXrefId(symbol);
        if (string.IsNullOrWhiteSpace(id))
            return;

        var normalized = NormalizeXrefIdForIndex(id);
        XrefToTargetPath[normalized] = GetTargetPathForLink(symbol);
    }

    private static string RenderMarkdownWithXrefs(string markdown, string currentDir)
    {
        markdown = MarkdownTemplate.Apply(markdown, TemplateValues);

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

                if (!ReportedBrokenXrefs.Add(rawId))
                    continue;

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
            IMacroDeclarationSymbol macro => $"M:{GetMacroDocName(macro)}",
            IPropertySymbol ps => $"P:{GetPropertyDocName(ps)}",
            IFieldSymbol fs => $"F:{GetFieldDocName(fs)}",
            IEventSymbol es => $"E:{GetEventDocName(es)}",
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

    private static string GetMacroDocName(IMacroDeclarationSymbol macro)
    {
        var sb = new StringBuilder();
        if (macro.ContainingNamespace is { IsGlobalNamespace: false } containingNamespace)
        {
            sb.Append(GetNamespaceFullName(containingNamespace));
            sb.Append('.');
        }

        sb.Append(macro.Name);
        if (macro.Arity > 0)
        {
            sb.Append("``");
            sb.Append(macro.Arity);
        }

        if (!macro.Parameters.IsEmpty)
        {
            sb.Append('(');
            for (var index = 0; index < macro.Parameters.Length; index++)
            {
                if (index > 0)
                    sb.Append(',');
                sb.Append(GetParamTypeDocName(macro.Parameters[index].Type));
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

    private static string GetEventDocName(IEventSymbol es)
        => $"{GetTypeDocName(es.ContainingType!)}.{es.Name}";

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

    private static IReadOnlyList<ITypeSymbol> GetInheritanceChain(ITypeSymbol typeSymbol)
    {
        var chain = new Stack<ITypeSymbol>();
        var current = typeSymbol;

        while (current is not null)
        {
            chain.Push(current);
            current = current.BaseType;
        }

        return chain.ToArray();
    }

    private static string FormatTypeLink(string currentDir, ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        var memberName = EscapeName(typeSymbol.ToDisplayString(format));
        if (!IsFromDocumentedAssembly(typeSymbol))
            return memberName;

        var target = GetTypeIndexPath(typeSymbol);
        return $"[{memberName}]({RelLink(currentDir, target)})";
    }

    private static void GenerateTypePage(Compilation compilation, ITypeSymbol typeSymbol)
    {
        var commentInfo = GetOrCreateDocInfo(typeSymbol);

        var indexPath = GetTypeIndexPath(typeSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        string name = typeSymbol.ToDisplayString(
            MemberDisplayFormat
                .WithKindOptions(SymbolDisplayKindOptions.None)
                .WithMemberOptions(SymbolDisplayMemberOptions.None));
        var signature = FormatSignature(typeSymbol);
        var heroHtml = SiteTemplate.RenderHero(
            RavenDocSymbolKind.Type,
            GetSymbolKindLabel(typeSymbol),
            name,
            signature);
        var metadataLines = new List<string>();

        if (typeSymbol.ContainingType is not null)
        {
            var containingType = typeSymbol.ContainingType!;
            metadataLines.Add(
                $"**Containing type**: {FormatTypeLink(currentDir, containingType, ContainingTypeDisplayFormat)}<br />");
        }
        if (typeSymbol.ContainingNamespace is not null)
        {
            var containingNamespace = typeSymbol.ContainingNamespace!;
            var target = GetNamespaceIndexPath(containingNamespace);
            var memberName = EscapeName(containingNamespace.ToDisplayString(ContainingNamespaceDisplayFormat));
            metadataLines.Add(
                $"**Namespace**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }

        metadataLines.AddRange(GetSourceAndAssemblyLines(compilation, typeSymbol));

        var relationshipLines = new List<string>();
        var inheritanceChain = GetInheritanceChain(typeSymbol);
        if (inheritanceChain.Count > 1)
        {
            var inheritanceLinks = inheritanceChain
                .Select(type => FormatTypeLink(currentDir, type, BaseTypeDisplayFormat));
            relationshipLines.Add(
                $"**Inheritance**: {string.Join(" → ", inheritanceLinks)}<br />");
        }
        var implementedInterfaces = typeSymbol.Interfaces
            .Distinct(SymbolEqualityComparer.Default)
            .OrderBy(type => type.ToDisplayString(BaseTypeDisplayFormat))
            .ToArray();
        if (implementedInterfaces.Length > 0)
        {
            var interfaceLinks = implementedInterfaces
                .Select(type => FormatTypeLink(currentDir, (ITypeSymbol)type, BaseTypeDisplayFormat));
            relationshipLines.Add(
                $"**Implements**: {string.Join(", ", interfaceLinks)}<br />");
        }

        var members = PreferDocumentableGenericDefinitions(typeSymbol.GetMembers())
            .Where(GetMembersFilterPredicate)
            .Where(x => x is not IMethodSymbol ms || ms.AssociatedSymbol is null)
            .Where(member => !IsUnionCaseProjectionArtifact(typeSymbol, member))
            .Where(CanRenderSymbol)
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .ToArray();

        var memberSections = new List<string>();
        if (typeSymbol is IUnionSymbol unionSymbol &&
            !unionSymbol.DeclaredCaseTypes.IsDefaultOrEmpty)
        {
            memberSections.Add(RenderUnionCaseSection(unionSymbol));
        }
        memberSections.AddRange(RenderGroupedMemberSections(
            currentDir,
            members,
            isNamespacePage: false));

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

        var contentMarkdown = ContentTemplate.RenderTypePage(
            new RavenDocTypeContentTemplateModel(
                heroHtml,
                metadataLines,
                relationshipLines,
                commentInfo.RawMarkdown,
                memberSections));
        var contentHtml = RenderMarkdownWithXrefs(contentMarkdown, currentDir);
        var pageHtml = WrapHtml(currentDir, name, documentedAssemblyName, contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }

    private static void GenerateMemberGroupPage(
        Compilation compilation,
        ITypeSymbol? containingType,
        string groupKey,
        IReadOnlyList<ISymbol> members)
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

        string name = members.Count == 1
            ? members[0].Name
            : groupName;
        var signature = members.Count == 1
            ? FormatSignature(members[0])
            : null;
        var heroHtml = SiteTemplate.RenderHero(
            GetTemplateSymbolKind(members[0]),
            members.Count == 1 ? GetSymbolKindLabel(members[0]) : "Member group",
            name,
            signature);
        var metadataLines = new List<string>();
        if (containingType is not null)
        {
            var target = GetTypeIndexPath(containingType);
            var memberName = EscapeName(containingType.ToDisplayString(ContainingTypeDisplayFormat));
            metadataLines.Add(
                $"**Type**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }
        else if (members[0].ContainingType is { } clrContainer)
        {
            var clrContainerName = clrContainer.ToDisplayString(
                SymbolDisplayFormat.FullyQualifiedFormat
                    .WithKindOptions(SymbolDisplayKindOptions.None));
            metadataLines.Add(
                $"**CLR container (for .NET interop)**: `{EscapeName(clrContainerName)}`<br />");
        }
        var containingNamespace = containingType?.ContainingNamespace ?? members[0].ContainingNamespace;
        if (containingNamespace is not null)
        {
            var target = GetNamespaceIndexPath(containingNamespace);
            var memberName = EscapeName(containingNamespace.ToDisplayString(ContainingNamespaceDisplayFormat));
            metadataLines.Add(
                $"**Namespace**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }

        metadataLines.AddRange(GetSourceAndAssemblyLines(compilation, members[0]));

        if (members.Count == 1)
        {
            var doc = GetOrCreateDocInfo(members[0]);
            var contentMarkdown = ContentTemplate.RenderMemberPage(
                new RavenDocMemberContentTemplateModel(
                    heroHtml,
                    metadataLines,
                    string.IsNullOrWhiteSpace(doc.RawMarkdown)
                        ? "_No documentation available._"
                        : doc.RawMarkdown));
            var htmlSingle = RenderMarkdownWithXrefs(contentMarkdown, currentDir);
            var pageSingle = WrapHtml(currentDir, name, documentedAssemblyName, htmlSingle);
            File.WriteAllText(filePath, pageSingle);
            return;
        }

        var variants = members
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .Select(member =>
            {
                var doc = GetOrCreateDocInfo(member);
                return new RavenDocMemberVariantTemplateModel(
                    EscapeName(member.Name),
                    SiteTemplate.RenderSignature(FormatSignature(member)),
                    GetSourceFileLine(member),
                    string.IsNullOrWhiteSpace(doc.RawMarkdown)
                        ? "_No documentation available._"
                        : doc.RawMarkdown);
            })
            .ToArray();
        var overloadsMarkdown = ContentTemplate.RenderMemberGroupPage(
            new RavenDocMemberGroupContentTemplateModel(
                heroHtml,
                metadataLines,
                variants));
        var overloadsHtml = RenderMarkdownWithXrefs(overloadsMarkdown, currentDir);
        var pageHtml2 = WrapHtml(currentDir, name, documentedAssemblyName, overloadsHtml);
        File.WriteAllText(filePath, pageHtml2);
    }

    private static void GenerateNamespacePage(Compilation compilation, INamespaceSymbol namespaceSymbol)
    {
        var docInfo = GetOrCreateDocInfo(namespaceSymbol);

        var indexPath = GetNamespaceIndexPath(namespaceSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        string name = namespaceSymbol.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat.WithKindOptions(SymbolDisplayKindOptions.None));
        var namespaceName = GetNamespaceFullName(namespaceSymbol);

        if (string.IsNullOrWhiteSpace(name))
            name = "Global namespace";

        var heroHtml = SiteTemplate.RenderHero(
            RavenDocSymbolKind.Namespace,
            "Namespace",
            name,
            string.IsNullOrWhiteSpace(namespaceName)
                ? null
                : $"namespace {name}");

        var declaredNamespaceMembers =
            PreferDocumentableGenericDefinitions(namespaceSymbol.GetMembers())
                .ToArray();
        var namespaceMemberContainers = declaredNamespaceMembers
            .OfType<INamedTypeSymbol>()
            .Where(IsNamespaceMemberContainer)
            .ToArray();
        var promotedNamespaceMembers = namespaceMemberContainers
            .SelectMany(static container => container.GetMembers())
            .Where(static member =>
                member.IsStatic &&
                member is not IMethodSymbol { MethodKind: MethodKind.Constructor })
            .Where(GetMembersFilterPredicate);
        var explicitAdditionalMembers = AdditionalNamespaceMembers.TryGetValue(
            namespaceName,
            out var documentedMembers)
            ? documentedMembers
            : [];
        var additionalMembers = promotedNamespaceMembers
            .Concat(explicitAdditionalMembers)
            .Distinct(SymbolEqualityComparer.Default)
            .ToArray();
        var interopContainers = additionalMembers
            .OfType<IMethodSymbol>()
            .Select(static method => method.ContainingType)
            .Where(static type => type is not null)
            .ToHashSet(SymbolEqualityComparer.Default);
        var members = declaredNamespaceMembers
            .Concat(additionalMembers)
            .Where(GetMembersFilterPredicate)
            .Where(CanRenderSymbol)
            .Where(member =>
                member is not ITypeSymbol type ||
                !interopContainers.Contains(type))
            .Distinct(SymbolEqualityComparer.Default)
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .Where(member =>
                additionalMembers.Contains(member, SymbolEqualityComparer.Default) ||
                member is INamespaceSymbol childNamespace &&
                NamespaceContainsDocumentableMembers(childNamespace) ||
                IsFromDocumentedAssembly(member))
            .ToArray();

        var memberSections = RenderGroupedMemberSections(
            currentDir,
            members,
            isNamespacePage: true);

        foreach (var ns2 in members.OfType<INamespaceSymbol>())
        {
            var childNamespaceName = GetNamespaceFullName(ns2);
            var hasAdditionalPublicMembers =
                AdditionalNamespaceMembers.TryGetValue(childNamespaceName, out var childMembers) &&
                childMembers.Any(GetMembersFilterPredicate);
            if (!hasAdditionalPublicMembers &&
                !NamespaceContainsDocumentableMembers(ns2))
                continue;

            GenerateNamespacePage(compilation, ns2);
        }

        foreach (var t2 in members.OfType<ITypeSymbol>())
        {
            GenerateTypePage(compilation, t2);
        }

        foreach (var m in members.Where(m => m is not INamespaceSymbol && m is not ITypeSymbol))
        {
            GenerateMemberGroupPage(
                compilation,
                IsAdditionalNamespaceMember(m) ? null : m.ContainingType,
                GetMemberGroupKey(m),
                new[] { m });
        }

        var contentMarkdown = ContentTemplate.RenderNamespacePage(
            new RavenDocNamespaceContentTemplateModel(
                heroHtml,
                docInfo.RawMarkdown,
                memberSections));
        var contentHtml = RenderMarkdownWithXrefs(contentMarkdown, currentDir);
        var pageHtml = WrapHtml(currentDir, name, documentedAssemblyName, contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }

    private static string EscapeName(string s)
    {
        return s.Replace("<", "&lt;").Replace(">", "&gt;");
    }

    private static string FormatSignature(ISymbol symbol)
    {
        var signature = symbol is IPropertySymbol property
            ? FormatPropertySignature(property)
            : OmitRedundantPublicModifier(symbol.ToDisplayString(MemberDisplayFormat));
        var typeParameters = symbol switch
        {
            INamedTypeSymbol type => type.TypeParameters,
            IMethodSymbol method => method.TypeParameters,
            IMacroDeclarationSymbol macro => macro.TypeParameters,
            _ => []
        };

        foreach (var parameter in typeParameters)
        {
            var marker = $"where {parameter.Name}:";
            if (signature.Contains(marker, StringComparison.Ordinal))
                continue;

            var constraints = new List<string>();
            var kind = parameter.ConstraintKind;
            if (kind.HasFlag(TypeParameterConstraintKind.ReferenceType))
                constraints.Add("class");
            if (kind.HasFlag(TypeParameterConstraintKind.ValueType))
                constraints.Add("struct");
            if (kind.HasFlag(TypeParameterConstraintKind.NotNull))
                constraints.Add("notnull");

            constraints.AddRange(parameter.ConstraintTypes.Select(type =>
                type.ToDisplayString(BaseTypeDisplayFormat)));

            if (kind.HasFlag(TypeParameterConstraintKind.Constructor))
                constraints.Add("new()");
            if (kind.HasFlag(TypeParameterConstraintKind.AllowByRefLike))
                constraints.Add("allows ref struct");

            if (constraints.Count > 0)
                signature += $" where {parameter.Name}: {string.Join(", ", constraints)}";
        }

        return signature;
    }

    private static string RenderUnionCaseSection(IUnionSymbol union)
    {
        var cases = union.DeclaredCaseTypes
            .OrderBy(static @case => @case.Ordinal)
            .Select(@case =>
            {
                var documentation = GetOrCreateDocInfo(@case);
                return new RavenDocCaseTemplateModel(
                    FormatUnionCaseSignature(@case),
                    documentation.Summary);
            })
            .ToArray();
        return SiteTemplate.RenderCaseSection(cases);
    }

    private static string FormatUnionCaseSignature(IUnionCaseTypeSymbol @case)
    {
        if (@case.ConstructorParameters.IsDefaultOrEmpty)
            return $"case {@case.Name}";

        var parameters = @case.ConstructorParameters.Select(parameter =>
            $"{parameter.Name}: {parameter.Type.ToDisplayString(BaseTypeDisplayFormat)}");
        return $"case {@case.Name}({string.Join(", ", parameters)})";
    }

    private static bool IsUnionCaseProjectionArtifact(
        ITypeSymbol declaringType,
        ISymbol member)
    {
        if (declaringType is not IUnionSymbol union ||
            member is not IMethodSymbol method ||
            method.MethodKind != MethodKind.Constructor &&
            !string.Equals(method.Name, "TryGetValue", StringComparison.Ordinal))
        {
            return false;
        }

        return method.Parameters.Any(parameter =>
            IsUnionCaseParameterType(parameter.Type, union));
    }

    private static bool IsUnionCaseParameterType(
        ITypeSymbol type,
        IUnionSymbol union)
    {
        if (type is IAddressTypeSymbol addressType)
            type = addressType.ReferencedType;

        if (type.IsUnionCase)
            return true;

        return union.DeclaredCaseTypes.Any(@case =>
            SymbolEqualityComparer.Default.Equals(type, @case) ||
            string.Equals(
                type.MetadataName,
                @case.MetadataName,
                StringComparison.Ordinal));
    }

    private static string FormatPropertySignature(IPropertySymbol property)
    {
        var propertyFormat = MemberDisplayFormat
            .WithKindOptions(
                MemberDisplayFormat.KindOptions &
                ~SymbolDisplayKindOptions.IncludeMemberKeyword)
            .WithMemberOptions(
                MemberDisplayFormat.MemberOptions &
                ~SymbolDisplayMemberOptions.IncludeAccessibility &
                ~SymbolDisplayMemberOptions.IncludeModifiers)
            .WithPropertyStyle(SymbolDisplayPropertyStyle.NameOnly);
        var coreSignature = property.ToDisplayString(propertyFormat);
        const string implicitInitSuffix = " { init; }";
        if (coreSignature.EndsWith(implicitInitSuffix, StringComparison.Ordinal))
            coreSignature = coreSignature[..^implicitInitSuffix.Length];

        var setter = property.SetMethod;
        var propertyKeyword =
            setter is { MethodKind: MethodKind.PropertySet, DeclaredAccessibility: Accessibility.Public }
                ? "var"
                : "val";
        var prefix = property.IsStatic ? $"static {propertyKeyword}" : propertyKeyword;
        var visibleAccessors = new List<string>();

        AppendVisibleAccessor(visibleAccessors, property.GetMethod, "get");
        AppendVisibleAccessor(
            visibleAccessors,
            setter,
            setter?.MethodKind == MethodKind.InitOnly ? "init" : "set");

        if (setter is { MethodKind: MethodKind.InitOnly, DeclaredAccessibility: Accessibility.Public })
            visibleAccessors.Add("init;");

        var accessorSuffix = visibleAccessors.Count == 0
            ? string.Empty
            : $" {{ {string.Join(" ", visibleAccessors)} }}";
        return $"{prefix} {coreSignature}{accessorSuffix}";
    }

    private static void AppendVisibleAccessor(
        List<string> accessors,
        IMethodSymbol? accessor,
        string keyword)
    {
        if (accessor is null || !IsProtectedAccessibility(accessor.DeclaredAccessibility))
            return;

        accessors.Add($"{GetAccessibilityDisplayText(accessor.DeclaredAccessibility)} {keyword};");
    }

    private static bool IsProtectedAccessibility(Accessibility accessibility)
        => accessibility is Accessibility.ProtectedAndProtected
            or Accessibility.ProtectedOrInternal
            or Accessibility.ProtectedAndInternal;

    private static string GetAccessibilityDisplayText(Accessibility accessibility)
        => accessibility switch
        {
            Accessibility.ProtectedAndProtected => "protected",
            Accessibility.ProtectedOrInternal => "protected internal",
            Accessibility.ProtectedAndInternal => "private protected",
            _ => string.Empty
        };

    private static string OmitRedundantPublicModifier(string signature)
    {
        const string publicPrefix = "public ";
        return signature.StartsWith(publicPrefix, StringComparison.Ordinal)
            ? signature[publicPrefix.Length..]
            : signature;
    }

    private static bool IsDocumentableSymbol(ISymbol symbol)
        => !IsProjectedUnionCaseType(symbol) &&
           !IsCompilerGeneratedExtensionArtifact(symbol) &&
           (symbol is INamespaceSymbol ||
            symbol.DeclaredAccessibility == Accessibility.Public);

    private static bool IsProjectedUnionCaseType(ISymbol symbol)
    {
        if (symbol is IUnionCaseTypeSymbol)
            return true;

        if (symbol is not ITypeSymbol type ||
            symbol.ContainingNamespace is not { } containingNamespace)
        {
            return false;
        }

        return containingNamespace.GetMembers()
            .OfType<IUnionSymbol>()
            .SelectMany(static union => union.DeclaredCaseTypes)
            .Any(@case =>
                SymbolEqualityComparer.Default.Equals(type, @case) ||
                string.Equals(
                    type.MetadataName,
                    @case.MetadataName,
                    StringComparison.Ordinal));
    }

    private static bool IsCompilerGeneratedExtensionArtifact(ISymbol symbol)
        => symbol.Name == ExtensionMarkerMethodName ||
           symbol is ITypeSymbol &&
           (symbol.Name.StartsWith(ExtensionGroupingTypePrefix, StringComparison.Ordinal) ||
            symbol.Name.StartsWith(ExtensionMarkerTypePrefix, StringComparison.Ordinal));

    private static IEnumerable<ISymbol> PreferDocumentableGenericDefinitions(
        IEnumerable<ISymbol> members)
    {
        var selected = new List<ISymbol>();
        var genericTypeIndexes = new Dictionary<string, int>(StringComparer.Ordinal);

        foreach (var member in members)
        {
            if (member is not INamedTypeSymbol { Arity: > 0 } type)
            {
                selected.Add(member);
                continue;
            }

            var key = string.Join(
                "|",
                GetNamespaceFullName(type.ContainingNamespace),
                type.ContainingType?.ToDisplayString(ContainingTypeDisplayFormat),
                type.Name,
                type.Arity);
            if (!genericTypeIndexes.TryGetValue(key, out var existingIndex))
            {
                genericTypeIndexes.Add(key, selected.Count);
                selected.Add(type);
                continue;
            }

            if (GetGenericDefinitionQuality(type) >
                GetGenericDefinitionQuality((INamedTypeSymbol)selected[existingIndex]))
            {
                selected[existingIndex] = type;
            }
        }

        return selected;
    }

    private static int GetGenericDefinitionQuality(INamedTypeSymbol type)
    {
        var display = type.ToDisplayString(MemberDisplayFormat);
        if (display.Contains("<>", StringComparison.Ordinal))
            return 0;

        return type.TypeParameters.IsDefaultOrEmpty ? 1 : 2;
    }

    private static bool IsFromDocumentedAssembly(ISymbol symbol)
    {
        if (documentedAssembly is null)
            return true;

        if (symbol is INamespaceSymbol namespaceSymbol)
            return namespaceSymbol.GetMembers().Any(IsFromDocumentedAssembly);

        return SymbolEqualityComparer.Default.Equals(symbol.ContainingAssembly, documentedAssembly);
    }

    private static bool IsAdditionalNamespaceMember(ISymbol symbol)
    {
        if (symbol.ContainingType is INamedTypeSymbol containingType &&
            IsNamespaceMemberContainer(containingType))
        {
            return true;
        }

        var namespaceName = GetNamespaceFullName(symbol.ContainingNamespace);
        return AdditionalNamespaceMembers.TryGetValue(namespaceName, out var members) &&
               members.Contains(symbol, SymbolEqualityComparer.Default);
    }

    private static bool IsNamespaceMemberContainer(INamedTypeSymbol type)
    {
        if (string.Equals(type.Name, "NamespaceMembers", StringComparison.Ordinal))
        {
            return true;
        }

        try
        {
            return type.GetAttributes().Any(static attribute =>
                string.Equals(
                    attribute.AttributeClass?.ToDisplayString(
                        SymbolDisplayFormat.FullyQualifiedFormat),
                    "System.Runtime.CompilerServices.TopLevelAttribute",
                    StringComparison.Ordinal));
        }
        catch (BadImageFormatException)
        {
            return false;
        }
    }

    private static bool NamespaceContainsDocumentableMembers(INamespaceSymbol namespaceSymbol)
    {
        foreach (var member in namespaceSymbol.GetMembers())
        {
            if (member is INamespaceSymbol childNamespace)
            {
                if (NamespaceContainsDocumentableMembers(childNamespace))
                    return true;
                continue;
            }

            if (IsDocumentableSymbol(member))
                return true;
        }

        var namespaceName = GetNamespaceFullName(namespaceSymbol);
        return AdditionalNamespaceMembers.TryGetValue(namespaceName, out var members) &&
               members.Any(GetMembersFilterPredicate);
    }

    private static INamespaceSymbol? GetOutermostNamespace(INamespaceSymbol? namespaceSymbol)
    {
        if (namespaceSymbol is null || namespaceSymbol.IsGlobalNamespace)
            return null;

        var current = namespaceSymbol;
        while (current.ContainingNamespace is { IsGlobalNamespace: false } parent)
            current = parent;
        return current;
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

public sealed record DocumentationSiteOptions(
    IReadOnlyList<DocumentationSiteLink> Links,
    IReadOnlyDictionary<string, string>? TemplateValues = null,
    string? SiteRootDirectory = null)
{
    public static DocumentationSiteOptions Empty { get; } = new([]);
}

public sealed record DocumentationSiteLink(string Label, string Url);
