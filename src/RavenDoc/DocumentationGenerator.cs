using System.Text;

using Markdig;
using Markdig.Extensions.AutoIdentifiers;
using Markdig.Syntax;
using Markdig.Syntax.Inlines;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

public static partial class DocumentationGenerator
{
    private static ApiContentTree ApiContent = new(null);
    private static DocumentationSourceLinks SourceLinks = new(null);
    private static IReadOnlyList<DocumentationNavigationItem>? PageNavigation;
    private static string? SharedNavigationFile;
    private static string? SharedNavigationVersion;

    private static string outputDir = "_docs";
    private static string documentedAssemblyName = "Raven";
    private static readonly HashSet<AssemblySymbol> DocumentedAssemblies = new(SymbolEqualityComparer.Default);

    private const string ExtensionGroupingTypePrefix = "<>__RavenExtensionGrouping_For_";
    private const string ExtensionMarkerTypePrefix = "<>__RavenExtensionMarker_";
    private const string ExtensionMarkerMethodName = "<Extension>$";

    private static readonly Func<Symbol, bool> GetMembersFilterPredicate =
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

    private static readonly Dictionary<Symbol, SymbolDocInfo> DocInfoCache
        = new(ReferenceEqualityComparer<Symbol>.Instance);

    // ----------------------------
    // XRef index: "T:Foo.Bar" -> absolute output path
    // (we normalize overload IDs to member-group pages)
    // ----------------------------

    private static readonly Dictionary<string, string> XrefToTargetPath
        = new(StringComparer.Ordinal);

    private static readonly HashSet<string> ReportedBrokenXrefs
        = new(StringComparer.Ordinal);

    private static readonly Dictionary<string, List<Symbol>> AdditionalNamespaceMembers
        = new(StringComparer.Ordinal);

    private static IReadOnlyList<DocumentationSiteLink> SiteLinks = [];
    private static DocumentationSiteOptions CurrentSiteOptions = DocumentationSiteOptions.Empty;
    private static bool UseExplicitLibraryDeclarations;
    private static readonly Dictionary<string, DocumentationNavigationItem> ApiNavigation = new(StringComparer.Ordinal);

    internal static IReadOnlyList<DocumentationNavigationItem> GetApiNavigation()
    {
        string ParentPath(string path)
        {
            if (CurrentSiteOptions.NamespaceNavigation == "flat" && ApiNavigation[path].Kind == "Namespace")
                return "";

            // Promote descendants past namespace rows omitted from navigation.
            var directory = Path.GetFileName(path) == "index.html"
                ? Path.GetDirectoryName(Path.GetDirectoryName(path)) : Path.GetDirectoryName(path);
            while (directory is not null)
            {
                var candidate = Path.Combine(directory, "index.html");
                if (ApiNavigation.ContainsKey(candidate))
                    return candidate;
                if (directory == outputDir)
                    break;
                directory = Path.GetDirectoryName(directory);
            }
            return "";
        }
        var children = ApiNavigation.Keys.ToLookup(ParentPath, StringComparer.Ordinal);
        DocumentationNavigationItem Build(string path) => ApiNavigation[path] with
        {
            Children = ApiNavigation[path].Kind is "Union" or "Enum" ? [] :
                children[path].OrderBy(child => ApiNavigation[child].Label, StringComparer.Ordinal)
                    .Select(Build).ToArray()
        };
        var roots = ApiNavigation.Keys.Where(path => !ApiNavigation.ContainsKey(ParentPath(path)))
            .OrderBy(path => ApiNavigation[path].Label, StringComparer.Ordinal).Select(Build).ToArray();
        return [new DocumentationNavigationItem("API reference",
            Path.GetRelativePath(SiteRootDirectory, Path.Combine(outputDir, "index.html")).Replace('\\', '/'), roots)];
    }
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
                    SymbolDisplayGenericsOptions.IncludeTypeConstraints |
                    SymbolDisplayGenericsOptions.IncludeVariance)
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
                .WithMiscellaneousOptions(SymbolDisplayFormat.FullyQualifiedFormat.MiscellaneousOptions & ~SymbolDisplayMiscellaneousOptions.UseSpecialTypes)
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
                    .OfType<MacroDeclarationSymbol>()
                    .Cast<Symbol>();
                var namespaceFunctions = tree.GetRoot()
                    .DescendantNodesAndSelf()
                    .OfType<FunctionStatementSyntax>()
                    .Where(function =>
                        function.Parent is CompilationUnitSyntax or BaseNamespaceDeclarationSyntax)
                    .Select(semanticModel.GetDeclaredSymbol)
                    .OfType<MethodSymbol>()
                    .Cast<Symbol>();
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
        AssemblySymbol assembly,
        string outputDir,
        DocumentationSiteOptions? siteOptions = null)
        => Process(
            compilation,
            assembly,
            assembly.GlobalNamespace,
            outputDir,
            additionalNamespaceMembers: [],
            siteOptions);

    internal static void ProcessAssemblies(
        Compilation compilation,
        IReadOnlyList<AssemblySymbol> assemblies,
        string outputDir,
        DocumentationSiteOptions siteOptions)
        => Process(compilation, assemblies[0], compilation.GlobalNamespace,
            outputDir, [], siteOptions, assemblies);

    private static void Process(
        Compilation compilation,
        AssemblySymbol assembly,
        NamespaceSymbol globalNamespace,
        string outputDir,
        IReadOnlyList<Symbol> additionalNamespaceMembers,
        DocumentationSiteOptions? siteOptions,
        IReadOnlyList<AssemblySymbol>? assemblies = null)
    {
        try { Directory.Delete(outputDir, recursive: true); } catch { }
        try { Directory.CreateDirectory(outputDir); } catch { }

        DocumentationGenerator.outputDir = outputDir;
        UseExplicitLibraryDeclarations = assemblies is not null;
        DocumentedAssemblies.Clear();
        DocumentedAssemblies.UnionWith(assemblies ?? [assembly]);
        documentedAssemblyName = assembly.Name;
        DocInfoCache.Clear();
        DocumentedTypes.Clear();
        ReverseTypeRelationships.Clear();
        XrefToTargetPath.Clear();
        ReportedBrokenXrefs.Clear();
        AdditionalNamespaceMembers.Clear();
        CurrentSiteOptions = siteOptions ?? DocumentationSiteOptions.Empty;
        ApiContent = new ApiContentTree(CurrentSiteOptions.ApiContent);
        SourceLinks = new DocumentationSourceLinks(CurrentSiteOptions.SourceRepository);
        MemberListStyle = CurrentSiteOptions.MemberListStyle;
        IncludedTypes = CurrentSiteOptions.Types?.ToHashSet(StringComparer.Ordinal);
        ExcludedMembers = CurrentSiteOptions.ExcludedMembers?.ToHashSet(StringComparer.Ordinal) ?? [];
        if (MemberListStyle is not ("compact" or "signatures"))
            throw new InvalidOperationException("memberListStyle must be compact or signatures.");
        if (CurrentSiteOptions.NamespaceNavigation is not ("hierarchical" or "flat"))
            throw new InvalidOperationException("namespaceNavigation must be hierarchical or flat.");
        if (CurrentSiteOptions.MemberGrouping is not ("kind" or "declaringType"))
            throw new InvalidOperationException("memberGrouping must be kind or declaringType.");
        ApiNavigation.Clear();
        PageNavigation = null;
        SharedNavigationFile = null;
        SharedNavigationVersion = null;
        CompanionOwners.Clear();
        PrepareExtensionLookup(compilation);
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
        {
            AddSymbolToXrefIndex(symbol);
            // Macro partition namespaces may not be present in the consumer assembly.
            for (var ns = symbol.ContainingNamespace; ns is { IsGlobalNamespace: false }; ns = ns.ContainingNamespace)
                AddSymbolToXrefIndex(ns);
        }

        if (CurrentSiteOptions.IndexOnly) return;
        if (CurrentSiteOptions.SharedXrefs is { } sharedXrefs)
            foreach (var (id, target) in sharedXrefs)
                XrefToTargetPath.TryAdd(id, target);

        BuildReverseTypeRelationships();

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
            .Cast<NamespaceSymbol>())
        {
            ProcessSymbol(compilation, namespaceRoot);
        }
        ApiContent.Validate();
    }

    private static void ProcessSymbol(Compilation compilation, Symbol symbol)
    {
        if (symbol is TypeSymbol typeSymbol)
        {
            GenerateTypePage(compilation, typeSymbol);
        }
        else if (symbol is NamespaceSymbol namespaceSymbol)
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
            DocumentationNavigation.ResolveLinks(SiteLinks, SiteRootDirectory, currentDir),
            RenderPageNavigation(currentDir),
            CurrentSiteOptions.ProjectName,
            DocumentationNavigation.Resolve(CurrentSiteOptions.Logo, SiteRootDirectory, currentDir),
            DocumentationNavigation.Resolve(CurrentSiteOptions.Stylesheet, SiteRootDirectory, currentDir),
            CurrentSiteOptions.Footer ?? "Raven documentation",
            Subtitle: CurrentSiteOptions.Subtitle,
            Notice: CurrentSiteOptions.Notice,
            ReleaseUrl: CurrentSiteOptions.ReleaseUrl,
            ReleaseLabel: CurrentSiteOptions.ReleaseLabel,
            ShowToc: CurrentSiteOptions.ShowToc,
            FaviconHref: DocumentationNavigation.Resolve(CurrentSiteOptions.Favicon, SiteRootDirectory, currentDir),
            GoogleAnalyticsId: CurrentSiteOptions.GoogleAnalyticsId));
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

    private static string RenderPageNavigation(string currentDirectory)
    {
        if (PageNavigation is null)
        {
            PageNavigation = DocumentationNavigation.Compose(CurrentSiteOptions.Navigation ?? [], GetApiNavigation());
            var fullNavigation = DocumentationNavigation.Render(PageNavigation, SiteRootDirectory, RootDir, "", CurrentSiteOptions.ModuleTerminology);
            if (CurrentSiteOptions.SharedApiNavigation)
            {
                SharedNavigationFile = Path.Combine(RootDir, "api-navigation.html");
                File.WriteAllText(SharedNavigationFile, fullNavigation);
                SharedNavigationVersion = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(
                    System.Text.Encoding.UTF8.GetBytes(fullNavigation)))[..16].ToLowerInvariant();
            }
        }
        if (SharedNavigationFile is null)
            return DocumentationNavigation.Render(PageNavigation, SiteRootDirectory, currentDirectory, moduleTerminology: CurrentSiteOptions.ModuleTerminology);

        // Namespace links remain usable without JavaScript or if the shared asset cannot load.
        DocumentationNavigationItem Fallback(DocumentationNavigationItem item) => item with
        {
            Children = item.Children?.Where(child => child.Kind is null or "Namespace").Select(Fallback).ToArray()
        };
        var overview = new DocumentationNavigationItem("API overview",
            Path.GetRelativePath(SiteRootDirectory, Path.Combine(RootDir, "index.html")).Replace('\\', '/'));
        var fallback = DocumentationNavigation.Render(new[] { overview }.Concat(PageNavigation.Select(Fallback)).ToArray(), SiteRootDirectory, currentDirectory, moduleTerminology: CurrentSiteOptions.ModuleTerminology);
        return fallback.Replace("<nav class=\"api-navigation-panel\"",
            "<nav data-navigation-src=\"" + HtmlEscape(RelLink(currentDirectory, SharedNavigationFile) + "?v=" + SharedNavigationVersion) + "\" class=\"api-navigation-panel\"");
    }

    private static string ToUrlPath(string path)
        => path.Replace(Path.DirectorySeparatorChar, '/');

    private static string GetNamespaceDir(NamespaceSymbol ns)
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

    private static string GetTypeDir(TypeSymbol type)
    {
        if (type is UnionCaseTypeSymbol { IsUnionCase: true } unionCase)
            return Path.Combine(GetTypeDir(unionCase.Union), unionCase.Name);

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

    private static string GetTypePathSegment(TypeSymbol type)
    {
        if (type is NamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            type = named.OriginalDefinition;

        if (type is NamedTypeSymbol nts && nts.Arity > 0)
            return $"{nts.Name}`{nts.Arity}";

        return type.Name;
    }

    private static string GetNamespaceIndexPath(NamespaceSymbol ns)
        => Path.Combine(GetNamespaceDir(ns), "index.html");

    private static string GetTypeIndexPath(TypeSymbol type)
        => Path.Combine(GetTypeDir(type), "index.html");

    private static string GetMemberGroupPath(Symbol member)
    {
        var typeDir = member.ContainingType is { } containingType &&
                      !IsAdditionalNamespaceMember(member)
            ? GetTypeDir(CompanionOwners.TryGetValue(containingType, out var union) ? union : containingType)
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
    // Grouping + filename helpers
    // ----------------------------

    private static string GetMemberGroupKey(Symbol member)
    {
        if (member is MethodSymbol ms)
        {
            if (IsOperatorLike(ms))
                return $"operator:{GetOperatorGroupName(ms)}";

            return $"method:{ms.Name}";
        }

        if (member is PropertySymbol ps)
        {
            if (ps.Parameters is { Length: > 0 })
                return $"indexer:{ps.Name}";

            return $"property:{ps.Name}";
        }

        if (member is FieldSymbol fs)
            return $"field:{fs.Name}";

        if (member is MacroDeclarationSymbol macro)
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
        TypeExtensions,
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
    MemberSectionKind.TypeExtensions,
    MemberSectionKind.Constants,
    MemberSectionKind.Fields,
    MemberSectionKind.Functions,
    MemberSectionKind.Macros,
    MemberSectionKind.Other
};

    private static string GetSectionTitle(MemberSectionKind kind) => kind switch
    {
        MemberSectionKind.Namespaces => CurrentSiteOptions.ModuleTerminology ? "Modules" : "Namespaces",
        MemberSectionKind.Types => "Types",
        MemberSectionKind.TypeExtensions => "Type extensions",
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

    private static bool IsOperatorLike(MethodSymbol ms)
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

    private static bool IsInvocationOperator(MethodSymbol ms)
    {
        return !ms.IsStatic && ms.Name is "self" or "Invoke";
    }

    private static string GetOperatorGroupName(MethodSymbol ms)
    {
        if (IsInvocationOperator(ms))
            return "self(...)";

        if (ms.MethodKind == MethodKind.Conversion)
            return GetConversionDisplayName(ms);

        return "operator " + GetOperatorToken(ms);
    }

    private static string GetConversionDisplayName(MethodSymbol method)
    {
        return method.Name switch
        {
            "op_Implicit" => "implicit conversion",
            "op_Explicit" => "explicit conversion",
            _ => "conversion"
        };
    }

    private static string GetOperatorToken(MethodSymbol method)
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

    private static MemberSectionKind GetMemberSectionForTypePage(Symbol m)
    {
        return m switch
        {
            TypeSymbol => MemberSectionKind.Types,

            FieldSymbol fs when fs.IsConst => MemberSectionKind.Constants,
            FieldSymbol => MemberSectionKind.Fields,

            MethodSymbol ms when ms.AssociatedSymbol is not null => MemberSectionKind.Other, // accessors filtered elsewhere
            MethodSymbol ms when ms.MethodKind == MethodKind.Constructor => MemberSectionKind.Constructors,
            MethodSymbol ms when IsOperatorLike(ms) => MemberSectionKind.Operators,
            MethodSymbol => MemberSectionKind.Methods,

            PropertySymbol ps when ps.Parameters.Length > 0 => MemberSectionKind.Indexers,
            PropertySymbol => MemberSectionKind.Properties,

            EventSymbol => MemberSectionKind.Events,

            _ => MemberSectionKind.Other
        };
    }

    private static MemberSectionKind GetMemberSectionForNamespacePage(Symbol m)
    {
        return m switch
        {
            NamespaceSymbol => MemberSectionKind.Namespaces,
            TypeSymbol type when IsExtensionContainer(type) => MemberSectionKind.TypeExtensions,
            TypeSymbol => MemberSectionKind.Types,
            FieldSymbol { IsConst: true } => MemberSectionKind.Constants,
            FieldSymbol => MemberSectionKind.Fields,
            MethodSymbol => MemberSectionKind.Functions,
            MacroDeclarationSymbol => MemberSectionKind.Macros,
            _ => MemberSectionKind.Other
        };
    }

    private static IReadOnlyList<string> RenderGroupedMemberSections(
        string currentDir,
        IEnumerable<Symbol> members,
        bool isNamespacePage,
        TypeSymbol? context = null,
        IReadOnlySet<Symbol>? specializedExtensions = null)
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
                RenderMemberTable(!isNamespacePage && section == MemberSectionKind.Types ? "Nested types" : GetSectionTitle(section), currentDir, ordered, context, specializedExtensions));
        }

        if (!isNamespacePage && renderedSections.Count > 0)
        {
            var inheritedControl = context is not null && IsStaticType(context) ? "" : "<label class=\"member-filter\" hidden><input id=\"show-inherited-members\" type=\"checkbox\" checked /> Show inherited members</label>";
            renderedSections.Insert(0, $"<div class=\"member-display-controls\"><label class=\"member-grouping\" hidden>Group members <select id=\"member-grouping\"><option value=\"kind\">By member kind</option><option value=\"declaringType\">By declaring type</option></select></label>{inheritedControl}<label class=\"member-filter\" hidden><input id=\"show-extension-members\" type=\"checkbox\" checked /> Show extension members</label></div>\n\n<div id=\"member-groups\" data-default-grouping=\"{CurrentSiteOptions.MemberGrouping}\">");
            renderedSections.Add("</div>");
        }
        return renderedSections;
    }

    // ----------------------------
    // Docs cache + summary extraction
    // ----------------------------

    private sealed class SymbolDocInfo
    {
        public required RavenDocumentation Documentation { get; init; }
        public string? RawMarkdown { get; init; }
        public string Summary { get; init; } = string.Empty;
    }

    private sealed class MemberRow
    {
        public required Symbol Symbol { get; init; }
        public required string Signature { get; init; }
        public required string Href { get; init; }
        public required string Summary { get; init; }
        public required RavenDocSymbolKind Kind { get; init; }
    }

    private static SymbolDocInfo GetOrCreateDocInfo(Symbol symbol)
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
            Documentation = documentation,
            RawMarkdown = markdown,
            Summary = System.Net.WebUtility.HtmlDecode(ExtractFirstParagraphSummary(markdown))
        };

        DocInfoCache[symbol] = info;
        return info;
    }

    private static string? DocumentationText(RavenDocumentation documentation, string? text)
        => documentation.SourceFormat == DocumentationFormat.Xml && text is not null
            ? System.Net.WebUtility.HtmlEncode(text)
            : text;

    private static string BuildDocumentationMarkdown(RavenDocumentation documentation, Symbol? member = null, string? currentDir = null, string? receiverMarkdown = null)
    {
        string? Section(DocumentationSectionKind kind) => DocumentationText(documentation, documentation.GetSection(kind));
        DocumentationAssociation[] Associations(DocumentationAssociationKind kind) => documentation.GetAssociations(kind)
            .Select(item => item with { Content = DocumentationText(documentation, item.Content) ?? "" }).ToArray();
        var builder = new StringBuilder();
        var details = Section(DocumentationSectionKind.Details);
        var remarks = Section(DocumentationSectionKind.Remarks);

        AppendDocumentationSection(
            builder,
            Section(DocumentationSectionKind.Summary));
        AppendDocumentationSection(builder, details);
        AppendDocumentationSection(builder, receiverMarkdown);
        AppendDocumentationAssociations(
            builder,
            "Type parameters",
            "Name",
            Associations(DocumentationAssociationKind.TypeParameter));
        if (member is not null && currentDir is not null)
            AppendMemberContract(builder, documentation, member, currentDir);
        else
        {
            AppendDocumentationAssociations(builder, "Parameters", "Name",
                Associations(DocumentationAssociationKind.Parameter));
            AppendNamedDocumentationSection(builder, "Returns", Section(DocumentationSectionKind.Result));
            AppendNamedDocumentationSection(builder, "Value", Section(DocumentationSectionKind.Value));
        }
        AppendNamedDocumentationSection(
            builder,
            "Remarks",
            string.Equals(details?.Trim(), remarks?.Trim(), StringComparison.Ordinal)
                ? null
                : remarks);
        AppendNamedDocumentationSection(
            builder,
            "Example",
            Section(DocumentationSectionKind.Example));
        AppendDocumentationAssociations(
            builder,
            "Errors",
            "Error",
            Associations(DocumentationAssociationKind.Error));
        AppendDocumentationAssociations(
            builder,
            "See also",
            "Reference",
            Associations(DocumentationAssociationKind.RelatedLink));

        return builder.ToString().Trim();
    }

    private static string GetMemberDocumentation(Symbol member, string currentDir)
    {
        var markdown = ApiContent.Merge(GetXrefId(member),
            BuildDocumentationMarkdown(GetOrCreateDocInfo(member).Documentation, member, currentDir));
        if (InterfaceImplementationStatus(member) is { } status)
            markdown = $"**Interface implementation**: {status}\n\n" + markdown;
        var origins = RenderMemberOrigins(currentDir, member);
        if (origins.Length > 0) markdown = origins + "\n\n" + markdown;
        return string.IsNullOrWhiteSpace(markdown) ? "_No documentation available._" : MarkdownTemplate.Apply(markdown, TemplateValues);
    }

    private static void AppendMemberContract(StringBuilder builder, RavenDocumentation documentation, Symbol member, string currentDir)
    {
        IEnumerable<ParameterSymbol> parameters = member switch
        {
            MethodSymbol method => method.Parameters,
            MacroDeclarationSymbol macro => macro.Parameters,
            PropertySymbol property => property.Parameters,
            _ => []
        };
        var descriptions = documentation.GetAssociations(DocumentationAssociationKind.Parameter);
        if (parameters.Any())
        {
            var table = new StringBuilder("| Name | Type | Description |\n| --- | --- | --- |\n");
            foreach (var parameter in parameters)
            {
                var description = DocumentationText(documentation, descriptions.FirstOrDefault(item => item.Name == parameter.Name)?.Content) ?? "";
                var passing = parameter.RefKind switch
                {
                    RefKind.Ref => "ref ",
                    RefKind.Out => "out ",
                    RefKind.In => "in ",
                    RefKind.RefReadOnly or RefKind.RefReadOnlyParameter => "ref readonly ",
                    _ => ""
                };
                table.AppendLine($"| <code class=\"api-parameter-name\">{EscapeName(parameter.Name)}</code> | {passing}{FormatContractType(currentDir, parameter.Type)} | {description.Replace("|", "\\|").Replace("\n", "<br />")} |");
            }
            AppendNamedDocumentationSection(builder, "Parameters", table.ToString());
        }

        var (heading, type, section) = member switch
        {
            PropertySymbol property => ("Property value", property.Type, DocumentationSectionKind.Value),
            FieldSymbol field => ("Field value", field.Type, DocumentationSectionKind.Value),
            EventSymbol @event => ("Event type", @event.Type, DocumentationSectionKind.Value),
            MethodSymbol method when !method.IsConstructor => ("Return value", method.ReturnType, DocumentationSectionKind.Result),
            MacroDeclarationSymbol macro => ("Return value", macro.ReturnType, DocumentationSectionKind.Result),
            _ => ("", (TypeSymbol?)null, DocumentationSectionKind.Result)
        };
        if (type is not null)
        {
            var description = DocumentationText(documentation, documentation.GetSection(section));
            AppendNamedDocumentationSection(builder, heading, FormatContractType(currentDir, type) +
                (string.IsNullOrWhiteSpace(description) ? "" : "\n\n" + description));
        }
    }

    private static string FormatContractType(string currentDir, TypeSymbol type, IReadOnlySet<string>? documentedTypeParameters = null)
    {
        if (type is TypeParameterSymbol parameter && documentedTypeParameters?.Contains(parameter.Name) == true)
            return $"[{EscapeName(parameter.Name)}](#type-parameters)";
        if (type.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
            return EscapeName(type.ToDisplayString(BaseTypeDisplayFormat));
        if (type is NamedTypeSymbol { TypeKind: TypeKind.Delegate } function
            && function.ContainingNamespace?.ToDisplayString() == "System"
            && function.Name is "Func" or "Action")
        {
            var arguments = function.TypeArguments;
            var parameters = function.Name == "Func" ? arguments.Take(arguments.Length - 1) : arguments;
            var parameterTypes = parameters.Select(parameter => FormatContractType(currentDir, parameter, documentedTypeParameters)).ToArray();
            var input = parameterTypes.Length == 1 ? parameterTypes[0] : "(" + string.Join(", ", parameterTypes) + ")";
            var result = function.Name == "Func" ? FormatContractType(currentDir, arguments[^1], documentedTypeParameters) : "()";
            return input + " -&gt; " + result;
        }
        if (type.GetNullableUnderlyingType() is { } underlying)
            return FormatContractType(currentDir, underlying, documentedTypeParameters) + "?";
        if (type is ArrayTypeSymbol array)
            return FormatContractType(currentDir, array.ElementType, documentedTypeParameters) + "\\[" +
                (array.IsFixedArray ? array.FixedLength?.ToString() ?? "" : new string(',', array.Rank - 1)) + "\\]";
        if (type is PointerTypeSymbol pointer)
            return FormatContractType(currentDir, pointer.PointedAtType, documentedTypeParameters) + "\\*";
        if (type is AddressTypeSymbol address)
            return "&amp;" + FormatContractType(currentDir, address.ReferencedType, documentedTypeParameters);
        if (type is not NamedTypeSymbol named)
            return EscapeName(type.ToDisplayString(ContainingTypeDisplayFormat));
        if (named.IsTupleType)
            return "(" + string.Join(", ", named.TupleElements.Select(element =>
                EscapeName(element.Name) + ": " + FormatContractType(currentDir, element.Type, documentedTypeParameters))) + ")";
        var name = EscapeName(named.Name);
        var definition = named.OriginalDefinition ?? named;
        if (XrefToTargetPath.TryGetValue(GetXrefId(definition), out var target))
            name = $"[{name}]({RelLink(currentDir, target)})";
        if (named.ContainingType is { } owner)
            name = FormatContractType(currentDir, owner, documentedTypeParameters) + "." + name;
        if (named.TypeArguments.Length > 0)
            name += "&lt;" + string.Join(", ", named.TypeArguments.Select(argument => FormatContractType(currentDir, argument, documentedTypeParameters))) + "&gt;";
        return name;
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

    private static string GetTargetPathForLink(Symbol symbol)
    {
        return symbol switch
        {
            NamespaceSymbol ns => GetNamespaceIndexPath(ns),
            TypeSymbol ts => GetTypeIndexPath(ts),
            MethodSymbol { ContainingType: UnionSymbol { IsUnion: true } union } method
                when IsUnionCaseProjectionArtifact(union, method) => GetTypeIndexPath(union),
            MethodSymbol { MethodKind: MethodKind.Constructor, ContainingType: UnionCaseTypeSymbol { IsUnionCase: true } @case }
                => GetMemberGroupPath(symbol),
            _ => GetMemberGroupPath(symbol),
        };
    }

    private static IReadOnlyList<MemberRow> BuildMemberRows(string currentDir, IEnumerable<Symbol> members)
    {
        var rows = new List<MemberRow>();

        foreach (var m in members)
        {
            var href = XrefToTargetPath.TryGetValue(NormalizeXrefIdForIndex(GetXrefId(m)), out var path)
                ? RelLink(currentDir, path) : "";

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

    private static bool CanRenderSymbol(Symbol symbol)
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

    private static string NavigationType(TypeSymbol type)
        => type.SpecialType is SpecialType.System_Void or SpecialType.System_Unit
            ? "()" : type.ToDisplayString(BaseTypeDisplayFormat.WithDelegateStyle(SymbolDisplayDelegateStyle.NameOnly));

    private static string NavigationParameters(IEnumerable<ParameterSymbol> parameters)
        => string.Join(", ", parameters.Select(parameter =>
        {
            var passing = parameter.RefKind switch
            {
                RefKind.Ref => "ref ",
                RefKind.Out => "out ",
                RefKind.In => "in ",
                RefKind.RefReadOnly or RefKind.RefReadOnlyParameter => "ref readonly ",
                _ => ""
            };
            var optional = parameter.IsOptional ? "?" : "";
            var variadic = parameter.IsVarParams ? "..." : "";
            return $"{parameter.Name}{optional}: {passing}{variadic}{NavigationType(parameter.Type)}";
        }));

    private static string GetNavigationName(Symbol symbol, bool receiverBound = false)
        => symbol switch
        {
            NamespaceSymbol ns => GetNamespaceFullName(ns),
            TypeSymbol type => GetTypeName(type),
            FieldSymbol field => field.Name + ": " + NavigationType(field.Type),
            PropertySymbol property => property.Name +
                (property.Parameters.Length > 0 ? $"[{NavigationParameters(property.Parameters)}]" : "") +
                ": " + NavigationType(property.Type),
            MethodSymbol method => GetNavigationMethod(method, receiverBound),
            _ => symbol.Name
        };

    private static string GetNavigationMethod(MethodSymbol method, bool receiverBound = false)
    {
        var name = method.IsConstructor ? method.ContainingType.Name
            : IsOperatorLike(method) ? GetOperatorGroupName(method) : method.Name;
        // Extension-container parameters are lifted onto imported methods. Match
        // names in the original receiver declaration, before lookup substitution.
        var receiverParameters = new HashSet<string>(StringComparer.Ordinal);
        void CollectReceiverParameters(TypeSymbol? type)
        {
            switch (type)
            {
                case TypeParameterSymbol parameter:
                    receiverParameters.Add(parameter.Name);
                    break;
                case ArrayTypeSymbol array:
                    CollectReceiverParameters(array.ElementType);
                    break;
                case NamedTypeSymbol named:
                    foreach (var argument in named.TypeArguments.IsEmpty
                        ? named.TypeParameters.Cast<TypeSymbol>() : named.TypeArguments)
                        CollectReceiverParameters(argument);
                    break;
            }
        }
        if (receiverBound)
            CollectReceiverParameters(method.OriginalDefinition.GetExtensionReceiverType());
        var typeParameters = method.TypeParameters.Where(parameter => !receiverParameters.Contains(parameter.Name)).ToArray();
        if (typeParameters.Length > 0)
            name += "<" + string.Join(", ", typeParameters.Select(parameter => parameter.Name)) + ">";
        var parameters = NavigationParameters(method.IsExtensionMethod ? method.Parameters.Skip(1) : method.Parameters);
        return $"{name}({parameters})" + (method.IsConstructor ? "" : $" -> {NavigationType(method.ReturnType)}");
    }

    private static Symbol BindNavigationReceiver(Symbol symbol, TypeSymbol? context)
    {
        if (symbol is not MethodSymbol method || context is null || !IsContributedExtension(symbol, context))
            return symbol;
        var pattern = method.OriginalDefinition.GetExtensionReceiverType();
        var bindings = new Dictionary<string, TypeSymbol>(StringComparer.Ordinal);
        bool Match(TypeSymbol? expected, TypeSymbol actual)
        {
            if (expected is TypeParameterSymbol parameter)
            {
                if (bindings.TryGetValue(parameter.Name, out var bound))
                    return SymbolEqualityComparer.Default.Equals(bound, actual);
                bindings[parameter.Name] = actual;
                return true;
            }
            if (expected is ArrayTypeSymbol leftArray && actual is ArrayTypeSymbol rightArray)
                return leftArray.Rank == rightArray.Rank && Match(leftArray.ElementType, rightArray.ElementType);
            if (expected is NamedTypeSymbol left && actual is NamedTypeSymbol right &&
                SymbolEqualityComparer.Default.Equals(left.OriginalDefinition, right.OriginalDefinition))
            {
                var leftArguments = left.TypeArguments.IsEmpty ? left.TypeParameters.Cast<TypeSymbol>().ToArray() : left.TypeArguments.ToArray();
                var rightArguments = right.TypeArguments.IsEmpty ? right.TypeParameters.Cast<TypeSymbol>().ToArray() : right.TypeArguments.ToArray();
                return leftArguments.Length == rightArguments.Length && leftArguments.Zip(rightArguments).All(pair => Match(pair.First, pair.Second));
            }
            return SymbolEqualityComparer.Default.Equals(expected, actual);
        }
        foreach (var receiver in GetInheritanceChain(context).Reverse().Concat(context.AllInterfaces))
        {
            bindings.Clear();
            if (!Match(pattern, receiver)) continue;
            if (method.TypeParameters.Length == 0) return method;
            return method.Construct(method.TypeParameters.Select(parameter =>
                bindings.TryGetValue(parameter.Name, out var argument) ? argument : parameter).ToArray());
        }
        return method;
    }

    private static string RenderMemberTable(
        string title,
        string currentDir,
        IEnumerable<Symbol> members,
        TypeSymbol? context = null,
        IReadOnlySet<Symbol>? specializedExtensions = null)
    {
        var rows = BuildMemberRows(currentDir, members);
        return SiteTemplate.RenderMemberSection(
            title,
            rows.Select(row => new RavenDocMemberTemplateModel(
                row.Kind,
                row.Signature,
                row.Href,
                row.Summary,
                specializedExtensions?.Contains(row.Symbol) == true ? GetNavigationName(row.Symbol, receiverBound: true) :
                row.Symbol is TypeSymbol extensionType && IsExtensionContainer(extensionType)
                    ? GetTypeName(extensionType) + " for " + string.Join(", ", GetExtensionReceivers(extensionType).Select(NavigationType))
                    : GetNavigationName(BindNavigationReceiver(row.Symbol, context), IsContributedExtension(row.Symbol, context)),
                row.Symbol switch
                {
                    MethodSymbol method => method.IsStatic && !method.IsExtensionMethod,
                    PropertySymbol property => property.IsStatic && !property.IsInstanceExtensionMember,
                    FieldSymbol field => field.IsStatic,
                    _ => false
                },
                RenderMemberOrigins(currentDir, row.Symbol, context) +
                    (specializedExtensions?.Contains(row.Symbol) == true ? RenderExtensionApplicability(row.Symbol) : ""),
                LogicalMemberOwner(row.Symbol) is { } owner ? GetNamespaceFullName(owner.ContainingNamespace) + "." + GetTypeName(owner) : "",
                context is not null && !IsDocumentationExtension(row.Symbol) && LogicalMemberOwner(row.Symbol) is { } declaringType && !SymbolEqualityComparer.Default.Equals(context, declaringType),
                IsDocumentationExtension(row.Symbol),
                IsContributedExtension(row.Symbol, context))).ToArray());
    }

    private static RavenDocSymbolKind GetTemplateSymbolKind(Symbol symbol)
    {
        return symbol switch
        {
            NamespaceSymbol => RavenDocSymbolKind.Namespace,
            UnionCaseTypeSymbol { IsUnionCase: true } => RavenDocSymbolKind.Case,
            UnionSymbol { IsUnion: true } => RavenDocSymbolKind.Union,
            TypeSymbol { TypeKind: TypeKind.Enum } => RavenDocSymbolKind.Enum,
            TypeSymbol { TypeKind: TypeKind.Delegate } => RavenDocSymbolKind.Delegate,
            TypeSymbol { TypeKind: TypeKind.Struct } => RavenDocSymbolKind.Struct,
            TypeSymbol { TypeKind: TypeKind.Interface } => RavenDocSymbolKind.Interface,
            TypeSymbol type when IsExtensionContainer(type) => RavenDocSymbolKind.Extension,
            TypeSymbol { TypeKind: TypeKind.Class } => RavenDocSymbolKind.Class,
            TypeSymbol => RavenDocSymbolKind.Type,
            MacroDeclarationSymbol => RavenDocSymbolKind.Macro,
            MethodSymbol method when IsOperatorLike(method) => RavenDocSymbolKind.Operator,
            MethodSymbol => RavenDocSymbolKind.Function,
            PropertySymbol => RavenDocSymbolKind.Property,
            FieldSymbol => RavenDocSymbolKind.Field,
            EventSymbol => RavenDocSymbolKind.Event,
            _ => RavenDocSymbolKind.Member
        };
    }

    private static string GetSymbolKindLabel(Symbol symbol)
    {
        return symbol switch
        {
            NamespaceSymbol => "Namespace",
            MacroDeclarationSymbol => "Macro",
            MethodSymbol method when IsOperatorLike(method) => "Operator",
            MethodSymbol { MethodKind: MethodKind.Constructor } => "Constructor",
            MethodSymbol when IsAdditionalNamespaceMember(symbol) => "Function",
            MethodSymbol => "Method",
            PropertySymbol { IsIndexer: true } => "Indexer",
            PropertySymbol => "Property",
            FieldSymbol { IsConst: true } => "Constant",
            FieldSymbol => "Field",
            EventSymbol => "Event",
            UnionCaseTypeSymbol { IsUnionCase: true } => "Union case",
            UnionSymbol { IsUnion: true } => "Union",
            TypeSymbol type when IsExtensionContainer(type) => "Type extension",
            TypeSymbol type when IsStaticType(type) => "Static class",
            TypeSymbol type => type.TypeKind.ToString(),
            _ => "Member"
        };
    }

    // ----------------------------
    // Definition location helpers
    // ----------------------------

    // Optional: filled from references so we can show the DLL path for metadata types.
    private static readonly Dictionary<string, string> AssemblyNameToPath
        = new(StringComparer.OrdinalIgnoreCase);

    private static string GetDefinedInText(Compilation compilation, Symbol symbol)
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
        Symbol symbol)
    {
        var lines = new List<string>();
        var asmFile = GetAssemblyFileNameForSymbol(compilation, symbol);
        lines.Add($"**Assembly**: {HtmlEscape(asmFile)}<br />");

        var sourceFileLine = GetSourceFileLine(symbol);
        if (sourceFileLine is not null)
            lines.Add(sourceFileLine);
        return lines;
    }

    private static string? GetSourceFileLine(Symbol symbol)
    {
        var links = SourceLinks.GetLinks(symbol);
        if (links.Count == 0) return null;
        return "**Source file**: " + string.Join(", ", links.Select(link => link.Url is { } url
            ? $"<a href=\"{HtmlEscape(url)}\">{HtmlEscape(link.File)}</a>"
            : HtmlEscape(link.File))) + "<br />";
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

    private static string GetAssemblyFileNameForSymbol(Compilation compilation, Symbol symbol)
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

    private static void BuildXrefIndex(NamespaceSymbol globalNamespace)
    {
        static void Visit(Symbol s)
        {
            if (!GetMembersFilterPredicate(s) || !IsFromDocumentedAssembly(s))
                return;

            AddSymbolToXrefIndex(s);

            if (s is UnionSymbol { IsUnion: true } union)
            {
                foreach (var companion in GetUnionCompanions(union))
                {
                    CompanionOwners[companion] = union;
                    XrefToTargetPath[GetXrefId(companion)] = GetTypeIndexPath(union);
                    foreach (var member in companion.GetMembers().Where(member => member is not TypeSymbol))
                        Visit(member);
                }
                foreach (var @case in union.DeclaredCaseTypes)
                {
                    Visit(@case);
                }
            }

            if (s is NamespaceOrTypeSymbol nts)
            {
                foreach (var m in PreferDocumentableGenericDefinitions(nts is NamespaceSymbol ns ? GetDocumentedNamespaceMembers(ns) : nts.GetMembers()))
                    Visit(m);
            }
        }

        Visit(globalNamespace);
    }

    private static void AddSymbolToXrefIndex(Symbol symbol)
    {
        if (!IsFromDocumentedAssembly(symbol) ||
            symbol is NamespaceSymbol ns && !NamespaceContainsDocumentableMembers(ns))
            return;

        var id = GetXrefId(symbol);
        if (string.IsNullOrWhiteSpace(id))
            return;

        var normalized = NormalizeXrefIdForIndex(id);
        XrefToTargetPath[normalized] = GetTargetPathForLink(symbol);
        if ((symbol is NamespaceSymbol || symbol is TypeSymbol || IsAdditionalNamespaceMember(symbol)) &&
            IsDocumentableSymbol(symbol) && CanRenderSymbol(symbol) &&
            (symbol is not NamespaceSymbol nsWithMembers || CurrentSiteOptions.ShowEmptyNamespaces ||
                NamespaceContainsDocumentableMembers(nsWithMembers, includeDescendants: false)) &&
            (symbol is not NamedTypeSymbol type || !IsNamespaceMemberContainer(type)))
        {
            if (symbol is NamedTypeSymbol documentedType)
                DocumentedTypes[GetXrefId(documentedType)] = documentedType;
            // Nested types remain indexed and listed on their containing type's page.
            if (symbol is TypeSymbol { ContainingType: not null })
                return;
            var target = GetTargetPathForLink(symbol);
            var label = symbol is NamespaceSymbol navigationNamespace
                ? (navigationNamespace.IsGlobalNamespace ? "API reference" : GetNamespaceFullName(navigationNamespace))
                : symbol is TypeSymbol navigationType ? GetTypeName(navigationType) : GetNavigationName(symbol);
            if (symbol is MethodSymbol && ApiNavigation.ContainsKey(target))
                label = symbol.Name + " (overloads)";
            ApiNavigation[target] = new DocumentationNavigationItem(label,
                Path.GetRelativePath(SiteRootDirectory, target).Replace('\\', '/'), Kind: GetSymbolKindLabel(symbol));
        }
    }

    internal static string ResolveArticleXref(string url, string currentDir, IReadOnlyDictionary<string, string>? targets = null)
    {
        targets ??= XrefToTargetPath;
        var id = NormalizeXrefIdIncoming(url["xref:".Length..]);
        if (targets.TryGetValue(id, out var target)) return RelLink(currentDir, target);
        foreach (var prefix in new[] { "T:", "N:", "M:", "P:", "F:", "E:" })
            if (targets.TryGetValue(prefix + id, out target)) return RelLink(currentDir, target);
        throw new InvalidOperationException($"Unresolved article xref: {url}");
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

    private static string GetXrefId(Symbol s)
    {
        if (s is null)
            return string.Empty;

        return s switch
        {
            NamespaceSymbol ns => ns.IsGlobalNamespace ? "" : $"N:{GetNamespaceFullName(ns)}",
            TypeSymbol ts => $"T:{GetTypeDocName(ts)}",
            MethodSymbol ms => $"M:{GetMethodDocName(ms)}",
            MacroDeclarationSymbol macro => $"M:{GetMacroDocName(macro)}",
            PropertySymbol ps => $"P:{GetPropertyDocName(ps)}",
            FieldSymbol fs => $"F:{GetFieldDocName(fs)}",
            EventSymbol es => $"E:{GetEventDocName(es)}",
            _ => ""
        };
    }

    private static string GetNamespaceFullName(NamespaceSymbol ns)
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

    private static string GetTypeDocName(TypeSymbol type)
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

        var chain = new Stack<TypeSymbol>();
        for (var cur = type; cur is not null; cur = cur.ContainingType)
            chain.Push(cur);

        bool first = true;
        while (chain.Count > 0)
        {
            var t = chain.Pop();
            if (!first)
                sb.Append('+');
            first = false;

            if (t is NamedTypeSymbol nts && nts.IsGenericType)
            {
                var def = (nts.IsUnboundGenericType || nts.Equals(nts.OriginalDefinition))
                    ? nts
                    : nts.OriginalDefinition as NamedTypeSymbol;

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

    private static string GetMemberOwnerDocPrefix(Symbol member)
    {
        if (member.ContainingType is { } type) return GetTypeDocName(type) + ".";
        return member.ContainingNamespace is { IsGlobalNamespace: false } ns ? GetNamespaceFullName(ns) + "." : "";
    }

    private static string GetMethodDocName(MethodSymbol ms)
    {
        // Type.Method(params...)
        var sb = new StringBuilder();
        sb.Append(GetMemberOwnerDocPrefix(ms));
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

    private static string GetMacroDocName(MacroDeclarationSymbol macro)
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

    private static string GetPropertyDocName(PropertySymbol ps)
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

    private static string GetFieldDocName(FieldSymbol fs)
        => GetMemberOwnerDocPrefix(fs) + fs.Name;

    private static string GetEventDocName(EventSymbol es)
        => $"{GetTypeDocName(es.ContainingType!)}.{es.Name}";

    private static string GetParamTypeDocName(TypeSymbol t)
    {
        // Minimal starter: use type doc name where possible, else fallback to fully qualified.
        if (t is NamedTypeSymbol nts)
            return GetTypeDocName(nts);

        return t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
    }

    // ----------------------------
    // Page generators
    // ----------------------------

    private static bool IsExtensionContainer(TypeSymbol type)
        => type.GetExtensionReceiverType() is not null ||
           (type.IsStatic && type.GetMembers().Any(member =>
               IsDocumentableSymbol(member) && IsDocumentationExtension(member)));

    private static IReadOnlyList<TypeSymbol> GetExtensionReceivers(TypeSymbol type)
    {
        if (!IsExtensionContainer(type))
            return [];
        var receivers = type.GetMembers()
            .Where(member => IsDocumentableSymbol(member) && IsDocumentationExtension(member))
            .Select(member => member.GetExtensionReceiverType()!)
            // Imported containers can lift the same receiver parameter onto each method.
            .DistinctBy(receiver => receiver.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat))
            .OrderBy(NavigationType, StringComparer.Ordinal)
            .ToArray();
        // A metadata container's receiver can be a representative first receiver.
        // Member receivers preserve distinct targets and substituted parameter names.
        return receivers.Length > 0 ? receivers
            : type.GetExtensionReceiverType() is { } receiver ? [receiver] : [];
    }

    private static bool IsStaticType(TypeSymbol type)
        => type.IsStatic || type.GetExtensionReceiverType() is not null;

    private static IReadOnlyList<TypeSymbol> GetInheritanceChain(TypeSymbol typeSymbol)
    {
        if (IsStaticType(typeSymbol)) return [];
        var chain = new Stack<TypeSymbol>();
        var current = typeSymbol;

        while (current is not null)
        {
            chain.Push(current);
            current = current.BaseType;
        }

        return chain.ToArray();
    }

    private static string FormatTypeLink(string currentDir, TypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        var memberName = EscapeName(GetTypeName(typeSymbol));
        var definition = typeSymbol is NamedTypeSymbol named ? named.OriginalDefinition : typeSymbol;
        if (!XrefToTargetPath.TryGetValue(GetXrefId(definition), out var target))
            return memberName;

        return $"[{memberName}]({RelLink(currentDir, target)})";
    }

    private static void GenerateTypePage(Compilation compilation, TypeSymbol typeSymbol)
    {
        var commentInfo = GetOrCreateDocInfo(typeSymbol);

        var indexPath = GetTypeIndexPath(typeSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        string name = GetTypeName(typeSymbol);
        var signature = FormatSignature(typeSymbol);
        var heroHtml = SiteTemplate.RenderHero(
            GetTemplateSymbolKind(typeSymbol),
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
                $"**{(CurrentSiteOptions.ModuleTerminology || containingNamespace.IsModule ? "Module" : "Namespace")}**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }

        var receivers = GetExtensionReceivers(typeSymbol);
        var documentedTypeParameters = commentInfo.Documentation.GetAssociations(DocumentationAssociationKind.TypeParameter)
            .Select(association => association.Name).OfType<string>().ToHashSet(StringComparer.Ordinal);
        var receiverMarkdown = receivers.Count == 0 ? null
            : $"## {(receivers.Count == 1 ? "Receiver" : "Receivers")}\n\n" +
              string.Join("\n\n", receivers.Select(receiver => FormatContractType(currentDir, receiver, documentedTypeParameters)));

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
        var implementedInterfaces = (IsStaticType(typeSymbol) ? [] : typeSymbol.Interfaces)
            .Distinct(SymbolEqualityComparer.Default)
            .OrderBy(type => type.ToDisplayString(BaseTypeDisplayFormat))
            .ToArray();
        if (implementedInterfaces.Length > 0)
        {
            var interfaceLinks = implementedInterfaces
                .Select(type => FormatTypeLink(currentDir, (TypeSymbol)type, BaseTypeDisplayFormat));
            relationshipLines.Add(
                $"**{(typeSymbol.TypeKind == TypeKind.Interface ? "Inherits" : "Implements")}**: {string.Join(", ", interfaceLinks)}<br />");
        }

        relationshipLines.AddRange(ClosedHierarchyLines(currentDir, typeSymbol));

        var members = PreferDocumentableGenericDefinitions(GetLogicalMembers(typeSymbol))
            .Where(GetMembersFilterPredicate)
            .Where(x => x is not MethodSymbol ms || ms.AssociatedSymbol is null)
            .Where(member => !IsUnionCaseProjectionArtifact(typeSymbol, member) && member is not UnionCaseTypeSymbol { IsUnionCase: true })
            .Where(CanRenderSymbol)
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .ToArray();

        var memberSections = new List<string>();
        memberSections.AddRange(RenderReverseTypeRelationships(currentDir, typeSymbol));
        if (typeSymbol is UnionSymbol { IsUnion: true } unionSymbol &&
            !unionSymbol.DeclaredCaseTypes.IsDefaultOrEmpty)
        {
            memberSections.Add(RenderUnionCaseSection(currentDir, unionSymbol));
            foreach (var unionCase in unionSymbol.DeclaredCaseTypes.Where(IsDocumentableSymbol))
                GenerateTypePage(compilation, unionCase);
        }
        var applicableExtensions = ApplicableExtensionMembers(typeSymbol).ToArray();
        var specializedExtensions = SpecializedExtensionMembers(typeSymbol, applicableExtensions).ToHashSet(SymbolEqualityComparer.Default);
        memberSections.AddRange(RenderGroupedMemberSections(
            currentDir,
            VisibleTypeMembers(typeSymbol, members).Concat(applicableExtensions).Concat(specializedExtensions),
            isNamespacePage: false, context: typeSymbol, specializedExtensions: specializedExtensions));


        foreach (var nestedType in members.OfType<TypeSymbol>())
        {
            GenerateTypePage(compilation, nestedType);
        }

        var groups = members
            .Where(m => m is not TypeSymbol)
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
                ApiContent.Merge(GetXrefId(typeSymbol), receiverMarkdown is null ? commentInfo.RawMarkdown
                    : MarkdownTemplate.Apply(BuildDocumentationMarkdown(commentInfo.Documentation, receiverMarkdown: receiverMarkdown), TemplateValues)),
                memberSections));
        var contentHtml = RenderMarkdownWithXrefs(contentMarkdown, currentDir);
        var pageHtml = WrapHtml(currentDir, name, documentedAssemblyName, contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }

    private static void GenerateMemberGroupPage(
        Compilation compilation,
        TypeSymbol? containingType,
        string groupKey,
        IReadOnlyList<Symbol> members)
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
                $"**Declaring type**: [{memberName}]({RelLink(currentDir, target)})<br />");
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
                $"**{(CurrentSiteOptions.ModuleTerminology || containingNamespace.IsModule ? "Module" : "Namespace")}**: [{memberName}]({RelLink(currentDir, target)})<br />");
        }

        metadataLines.AddRange(GetSourceAndAssemblyLines(compilation, members[0]));

        if (members.Count == 1)
        {
            var contentMarkdown = ContentTemplate.RenderMemberPage(
                new RavenDocMemberContentTemplateModel(
                    heroHtml,
                    metadataLines,
                    GetMemberDocumentation(members[0], currentDir)));
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
                return new RavenDocMemberVariantTemplateModel(
                    EscapeName(member.Name),
                    SiteTemplate.RenderSignature(FormatSignature(member)),
                    GetSourceFileLine(member),
                    GetMemberDocumentation(member, currentDir));
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

    private static void GenerateNamespacePage(Compilation compilation, NamespaceSymbol namespaceSymbol)
    {
        var docInfo = GetOrCreateDocInfo(namespaceSymbol);

        var indexPath = GetNamespaceIndexPath(namespaceSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        string name = namespaceSymbol.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat.WithKindOptions(SymbolDisplayKindOptions.None));
        var namespaceName = GetNamespaceFullName(namespaceSymbol);

        if (namespaceSymbol.IsGlobalNamespace)
            name = CurrentSiteOptions.ApiDisplayName ?? (CurrentSiteOptions.ModuleTerminology ? "Global module" : "Global namespace");

        var heroHtml = SiteTemplate.RenderHero(
            RavenDocSymbolKind.Namespace,
            namespaceSymbol.IsGlobalNamespace && CurrentSiteOptions.ApiDisplayName is not null ? "API reference" : (CurrentSiteOptions.ModuleTerminology || namespaceSymbol.IsModule ? "Module" : "Namespace"),
            name,
            string.IsNullOrWhiteSpace(namespaceName)
                ? null
                : $"{(CurrentSiteOptions.ModuleTerminology || namespaceSymbol.IsModule ? "module" : "namespace")} {name}");

        var declaredNamespaceMembers =
            PreferDocumentableGenericDefinitions(GetDocumentedNamespaceMembers(namespaceSymbol))
                .Where(member => !IsProjectedUnionCaseType(member))
                .ToArray();
        var namespaceMemberContainers = declaredNamespaceMembers
            .OfType<NamedTypeSymbol>()
            .Where(IsNamespaceMemberContainer)
            .ToArray();
        var promotedNamespaceMembers = namespaceMemberContainers
            .SelectMany(static container => container.GetMembers())
            .Where(static member =>
                member.IsStatic &&
                member is not MethodSymbol { MethodKind: MethodKind.Constructor })
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
            .OfType<MethodSymbol>()
            .Select(static method => method.ContainingType)
            .Where(static type => type is not null)
            .ToHashSet(SymbolEqualityComparer.Default);
        var members = declaredNamespaceMembers
            .Concat(additionalMembers)
            .Where(GetMembersFilterPredicate)
            .Where(CanRenderSymbol)
            .Where(member =>
                member is not TypeSymbol type ||
                !interopContainers.Contains(type))
            .Distinct(SymbolEqualityComparer.Default)
            .OrderBy(m => m.Name)
            .ThenBy(m => m.ToDisplayString(MemberDisplayFormat))
            .Where(member => member is NamespaceSymbol childNamespace
                ? NamespaceContainsDocumentableMembers(childNamespace)
                : additionalMembers.Contains(member, SymbolEqualityComparer.Default) ||
                  IsFromDocumentedAssembly(member))
            .ToArray();

        var memberSections = RenderGroupedMemberSections(
            currentDir,
            members,
            isNamespacePage: true);

        foreach (var ns2 in members.OfType<NamespaceSymbol>())
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

        foreach (var t2 in members.OfType<TypeSymbol>())
        {
            GenerateTypePage(compilation, t2);
        }

        foreach (var group in members.Where(m => m is not NamespaceSymbol && m is not TypeSymbol).GroupBy(GetMemberGroupKey))
        {
            var first = group.First();
            GenerateMemberGroupPage(
                compilation,
                IsAdditionalNamespaceMember(first) ? null : first.ContainingType,
                group.Key,
                group.ToArray());
        }

        var contentMarkdown = ContentTemplate.RenderNamespacePage(
            new RavenDocNamespaceContentTemplateModel(
                heroHtml,
                ApiContent.Merge(GetXrefId(namespaceSymbol), docInfo.RawMarkdown),
                memberSections));
        var contentHtml = RenderMarkdownWithXrefs(contentMarkdown, currentDir);
        var pageHtml = WrapHtml(currentDir, name, documentedAssemblyName, contentHtml);
        File.WriteAllText(indexPath, pageHtml);
    }

    private static string EscapeName(string s)
    {
        return s.Replace("<", "&lt;").Replace(">", "&gt;");
    }

    private static string GetTypeName(TypeSymbol type)
    {
        if (type is not NamedTypeSymbol named)
            return type.ToDisplayString(BaseTypeDisplayFormat);
        if (type is UnionCaseTypeSymbol { IsUnionCase: true })
            return type.Name;
        var arguments = named.TypeArguments;
        var parameters = named.TypeParameters;
        var suffix = arguments.Length > 0
            ? string.Join(", ", arguments.Select(GetTypeName))
            : string.Join(", ", parameters.Select(parameter => parameter.Name));
        return named.Name + (named.Arity > 0 || arguments.Length > 0 ? "<" + suffix + ">" : "");
    }

    private static string FormatSignature(Symbol symbol)
    {
        if (symbol is UnionCaseTypeSymbol { IsUnionCase: true } unionCase)
            return FormatUnionCaseSignature(unionCase);
        if (symbol is NamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType &&
            delegateType.GetDelegateInvokeMethod() is { } invoke)
            return $"delegate {GetTypeName(delegateType)}({NavigationParameters(invoke.Parameters)}) -> {NavigationType(invoke.ReturnType)}";

        var extensionReceivers = symbol is TypeSymbol typeSymbol ? GetExtensionReceivers(typeSymbol) : [];
        var signature = extensionReceivers.Count == 1 && symbol is TypeSymbol extensionType
            ? $"extension {GetTypeName(extensionType)} for {NavigationType(extensionReceivers[0])}"
            : extensionReceivers.Count > 1
            ? "" // A container with several receivers has no single extension declaration.
            : symbol is MethodSymbol { IsExtensionMethod: true } extensionMethod
            ? FormatInstanceExtensionSignature(extensionMethod)
            : symbol is PropertySymbol property
            ? FormatPropertySignature(property)
            : OmitRedundantPublicModifier(symbol.ToDisplayString(MemberDisplayFormat));
        if (symbol is TypeSymbol staticType && !IsExtensionContainer(staticType) && IsStaticType(staticType) && !signature.StartsWith("static ", StringComparison.Ordinal))
            signature = "static " + signature;
        var typeParameters = symbol switch
        {
            NamedTypeSymbol type => type.TypeParameters,
            MethodSymbol method => method.TypeParameters,
            MacroDeclarationSymbol macro => macro.TypeParameters,
            _ => []
        };

        foreach (var parameter in typeParameters)
        {
            var marker = $"where {parameter.Name}:";
            if (signature.Contains(marker, StringComparison.Ordinal))
                continue;

            if (FormatTypeParameterConstraint(parameter) is { } constraint)
                signature += " " + constraint;
        }

        return signature;
    }

    private static string? FormatTypeParameterConstraint(TypeParameterSymbol parameter)
    {
        var constraints = new List<string>();
        var kind = parameter.ConstraintKind;
        if (kind.HasFlag(TypeParameterConstraintKind.FunctionType))
            constraints.Add("func");
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

        return constraints.Count > 0 ? $"where {parameter.Name}: {string.Join(", ", constraints)}" : null;
    }

    private static string RenderUnionCaseSection(string currentDir, UnionSymbol union)
    {
        var cases = union.DeclaredCaseTypes
            .Where(IsDocumentableSymbol)
            .OrderBy(static @case => @case.Ordinal)
            .Select(@case =>
            {
                var documentation = GetOrCreateDocInfo(@case);
                return new RavenDocCaseTemplateModel(
                    FormatUnionCaseSignature(@case),
                    documentation.Summary,
                    RelLink(currentDir, GetTypeIndexPath(@case)));
            })
            .ToArray();
        return SiteTemplate.RenderCaseSection(cases);
    }

    private static string FormatUnionCaseSignature(UnionCaseTypeSymbol @case)
    {
        if (@case.ConstructorParameters.IsDefaultOrEmpty)
            return $"case {@case.Name}";

        var parameters = @case.ConstructorParameters.Select(parameter =>
            $"{parameter.Name}: {parameter.Type.ToDisplayString(BaseTypeDisplayFormat)}");
        return $"case {@case.Name}({string.Join(", ", parameters)})";
    }

    private static bool IsUnionCaseProjectionArtifact(
        TypeSymbol declaringType,
        Symbol member)
    {
        if (declaringType is not UnionSymbol { IsUnion: true } union ||
            member is not MethodSymbol method ||
            method.MethodKind != MethodKind.Constructor &&
            !string.Equals(method.Name, "TryGetValue", StringComparison.Ordinal))
        {
            return false;
        }

        return method.Parameters.Any(parameter =>
            IsUnionCaseParameterType(parameter.Type, union));
    }

    private static bool IsUnionCaseParameterType(
        TypeSymbol type,
        UnionSymbol union)
    {
        if (type is AddressTypeSymbol addressType)
            type = addressType.ReferencedType;

        if (type.IsUnionCase)
            return true;

        return union.DeclaredCaseTypes.Any(@case =>
            SymbolEqualityComparer.Default.Equals(type, @case) ||
            string.Equals(
                GetTypeDocName(type),
                GetTypeDocName(@case),
                StringComparison.Ordinal));
    }

    private static string FormatInstanceExtensionSignature(MethodSymbol method)
    {
        // CLR extension receivers are parameters of static carrier methods. The
        // documentation declaration describes the Raven instance member instead.
        var nameFormat = MemberDisplayFormat
            .WithMemberOptions(MemberDisplayFormat.MemberOptions &
                ~SymbolDisplayMemberOptions.IncludeParameters &
                ~SymbolDisplayMemberOptions.IncludeType &
                ~SymbolDisplayMemberOptions.IncludeModifiers)
            .WithDelegateStyle(SymbolDisplayDelegateStyle.NameOnly);
        var name = OmitRedundantPublicModifier(method.ToDisplayString(nameFormat));
        if (method.IsAsync)
            name = "async " + name;
        var parameters = method.IsExtensionMethod ? method.Parameters.Skip(1) : method.Parameters;
        return $"{name}({string.Join(", ", parameters.Select(parameter => parameter.ToDisplayString(MemberDisplayFormat)))}) -> {NavigationType(method.ReturnType)}";
    }

    private static string FormatPropertySignature(PropertySymbol property)
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
        var prefix = property.IsStatic && !property.IsInstanceExtensionMember ? $"static {propertyKeyword}" : propertyKeyword;
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
        MethodSymbol? accessor,
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

    internal static string MemberListStyle { get; private set; } = "compact";
    private static HashSet<string>? IncludedTypes { get; set; }
    private static HashSet<string> ExcludedMembers { get; set; } = [];

    public static Dictionary<string, string> ExportXrefs(string root)
        => XrefToTargetPath.ToDictionary(pair => pair.Key,
            pair => Path.GetRelativePath(root, pair.Value).Replace('\\', '/'));

    private static bool IsSelected(Symbol symbol)
        => IncludedTypes is null ||
           symbol.ContainingType is { } companion && CompanionOwners.TryGetValue(companion, out var owner) && IsSelected(owner) ||
           symbol is UnionCaseTypeSymbol { IsUnionCase: true } selectedCase && IsSelected(selectedCase.Union) ||
           symbol.ContainingType is UnionCaseTypeSymbol { IsUnionCase: true } caseOwner && IsSelected(caseOwner.Union) ||
           symbol.ContainingType is { } containing && IsSelected(containing) ||
           symbol is NamespaceSymbol ns && (ns.IsGlobalNamespace ||
               IncludedTypes.Any(type => type.StartsWith(GetNamespaceFullName(ns) + ".", StringComparison.Ordinal))) ||
           symbol is not NamespaceSymbol && (symbol as TypeSymbol ?? symbol.ContainingType) is { } selectedType &&
           IncludedTypes.Contains(GetTypeDocName(selectedType).Replace('+', '.'));

    private static bool IsDocumentableSymbol(Symbol symbol)
        => IsSelected(symbol) &&
           !ExcludedMembers.Contains(GetXrefId(symbol).Replace('+', '.').Replace("..ctor", ".#ctor")) &&
           !IsCompilerGeneratedExtensionArtifact(symbol) &&
           (symbol is NamespaceSymbol ||
            symbol.DeclaredAccessibility is Accessibility.Public
                or Accessibility.ProtectedAndProtected
                or Accessibility.ProtectedOrInternal);

    private static bool IsProjectedUnionCaseType(Symbol symbol)
    {
        if (symbol is UnionCaseTypeSymbol { IsUnionCase: true })
            return true;

        if (symbol is not TypeSymbol type ||
            symbol.ContainingNamespace is not { } containingNamespace)
        {
            return false;
        }

        return containingNamespace.GetMembers()
            .OfType<UnionSymbol>()
            .SelectMany(static union => union.DeclaredCaseTypes)
            .Any(@case =>
                SymbolEqualityComparer.Default.Equals(type, @case) ||
                string.Equals(
                    GetTypeDocName(type),
                    GetTypeDocName(@case),
                    StringComparison.Ordinal));
    }

    private static bool IsCompilerGeneratedExtensionArtifact(Symbol symbol)
        => symbol.Name == ExtensionMarkerMethodName ||
           symbol is TypeSymbol &&
           (symbol.Name.StartsWith(ExtensionGroupingTypePrefix, StringComparison.Ordinal) ||
            symbol.Name.StartsWith(ExtensionMarkerTypePrefix, StringComparison.Ordinal));

    private static IEnumerable<Symbol> PreferDocumentableGenericDefinitions(
        IEnumerable<Symbol> members)
    {
        var selected = new List<Symbol>();
        var genericTypeIndexes = new Dictionary<string, int>(StringComparer.Ordinal);

        foreach (var member in members)
        {
            if (member is not NamedTypeSymbol { Arity: > 0 } type)
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
                GetGenericDefinitionQuality((NamedTypeSymbol)selected[existingIndex]))
            {
                selected[existingIndex] = type;
            }
        }

        return selected;
    }

    private static int GetGenericDefinitionQuality(NamedTypeSymbol type)
    {
        var display = type.ToDisplayString(MemberDisplayFormat);
        if (display.Contains("<>", StringComparison.Ordinal))
            return 0;

        return type.TypeParameters.IsDefaultOrEmpty ? 1 : 2;
    }

    private static IEnumerable<Symbol> GetDocumentedNamespaceMembers(NamespaceSymbol namespaceSymbol)
    {
        if (!UseExplicitLibraryDeclarations) return namespaceSymbol.GetMembers();
        // A consumer's merged namespace may collapse equivalent primitive symbols
        // from its bootstrap and the documented library. Render the library's own
        // declarations, regardless of reference enumeration order.
        var members = namespaceSymbol.GetMembers().Where(member => member is NamespaceSymbol).ToList();
        var name = GetNamespaceFullName(namespaceSymbol);
        foreach (var assembly in DocumentedAssemblies)
        {
            IEnumerable<NamespaceSymbol> current = [assembly.GlobalNamespace];
            foreach (var segment in name.Split('.', StringSplitOptions.RemoveEmptyEntries))
                current = current.SelectMany(owner => owner.GetMembers(segment).OfType<NamespaceSymbol>()).ToArray();
            members.AddRange(current.SelectMany(owner => owner.GetMembers()).Where(member => member is not NamespaceSymbol));
        }
        return members.Distinct(ReferenceEqualityComparer<Symbol>.Instance);
    }

    private static bool IsFromDocumentedAssembly(Symbol symbol)
    {
        if (symbol is NamespaceSymbol namespaceSymbol)
        {
            var name = GetNamespaceFullName(namespaceSymbol);
            return AdditionalNamespaceMembers.Keys.Any(key => name.Length == 0 || key == name ||
                       key.StartsWith(name + ".", StringComparison.Ordinal)) ||
                   GetDocumentedNamespaceMembers(namespaceSymbol).Any(IsFromDocumentedAssembly);
        }

        // Explicit source members can belong to the separate compile-time macro assembly.
        var namespaceName = GetNamespaceFullName(symbol.ContainingNamespace);
        return AdditionalNamespaceMembers.TryGetValue(namespaceName, out var members) &&
                   members.Contains(symbol, SymbolEqualityComparer.Default) ||
               symbol.ContainingAssembly is { } containingAssembly && DocumentedAssemblies.Contains(containingAssembly);
    }

    private static bool IsAdditionalNamespaceMember(Symbol symbol)
    {
        if (symbol is MethodSymbol or FieldSymbol && symbol.ContainingType is null &&
            symbol.ContainingSymbol is NamespaceSymbol)
            return true;

        if (symbol.ContainingType is NamedTypeSymbol containingType &&
            IsNamespaceMemberContainer(containingType))
        {
            return true;
        }

        var namespaceName = GetNamespaceFullName(symbol.ContainingNamespace);
        return AdditionalNamespaceMembers.TryGetValue(namespaceName, out var members) &&
               members.Contains(symbol, SymbolEqualityComparer.Default);
    }

    private static bool IsNamespaceMemberContainer(NamedTypeSymbol type)
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

    private static bool NamespaceContainsDocumentableMembers(NamespaceSymbol namespaceSymbol, bool includeDescendants = true)
    {
        foreach (var member in GetDocumentedNamespaceMembers(namespaceSymbol))
        {
            if (member is NamespaceSymbol childNamespace)
            {
                if (includeDescendants && NamespaceContainsDocumentableMembers(childNamespace))
                    return true;
                continue;
            }

            if (!IsDocumentableSymbol(member) || !IsFromDocumentedAssembly(member))
                continue;
            if (member is NamedTypeSymbol container && IsNamespaceMemberContainer(container))
            {
                if (container.GetMembers().Any(promoted => promoted.IsStatic &&
                    promoted is not MethodSymbol { MethodKind: MethodKind.Constructor } &&
                    IsDocumentableSymbol(promoted) && CanRenderSymbol(promoted)))
                    return true;
            }
            else if (CanRenderSymbol(member))
                return true;
        }

        var namespaceName = GetNamespaceFullName(namespaceSymbol);
        return AdditionalNamespaceMembers.TryGetValue(namespaceName, out var members) &&
               members.Any(GetMembersFilterPredicate);
    }

    private static NamespaceSymbol? GetOutermostNamespace(NamespaceSymbol? namespaceSymbol)
    {
        if (namespaceSymbol is null || namespaceSymbol.IsGlobalNamespace)
            return null;

        var current = namespaceSymbol;
        while (current.ContainingNamespace is { IsGlobalNamespace: false } parent)
            current = parent;
        return current;
    }

    // ----------------------------
    // Reference equality comparer (so we can use Symbol keys safely)
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
    string? SiteRootDirectory = null,
    string? ProjectName = null,
    string? Logo = null,
    string? Stylesheet = null,
    IReadOnlyList<DocumentationNavigationItem>? Navigation = null,
    string? Footer = null,
    string MemberListStyle = "compact",
    IReadOnlyList<string>? Types = null,
    IReadOnlyList<string>? ExcludedMembers = null,
    string? Subtitle = null,
    string? Notice = null,
    string? ReleaseUrl = null,
    string? ReleaseLabel = null,
    bool ShowToc = true,
    string? Favicon = null,
    string NamespaceNavigation = "hierarchical",
    string? GoogleAnalyticsId = null,
    bool ShowEmptyNamespaces = false,
    IReadOnlyList<string>? ExtensionNamespaces = null,
    IReadOnlyList<string>? ExtensionMembers = null,
    string? ApiContent = null,
    string MemberGrouping = "kind",
    string? ApiDisplayName = null,
    DocumentationSourceRepository? SourceRepository = null,
    bool SharedApiNavigation = false,
    bool ModuleTerminology = false)
{
    internal bool IndexOnly { get; init; }
    internal IReadOnlyDictionary<string, string>? SharedXrefs { get; init; }

    public static DocumentationSiteOptions Empty { get; } = new([]);
}

public sealed record DocumentationSiteLink(string Label, string Url = "", IReadOnlyList<DocumentationSiteLink>? Children = null);
