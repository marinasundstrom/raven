using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

return RavenDocCommand.Run(args);

internal static class RavenDocCommand
{
    private const string DefaultTargetFramework = "net10.0";

    public static int Run(string[] args)
    {
        if (!TryParseArguments(args, out var options))
            return 1;

        if (options.ShowHelp)
        {
            PrintHelp();
            return 0;
        }

        try
        {
            var inputPath = Path.GetFullPath(options.InputPath!);
            if (!File.Exists(inputPath) && !Directory.Exists(inputPath))
            {
                Console.Error.WriteLine($"RavenDoc input does not exist: {inputPath}");
                return 1;
            }

            var outputPath = Path.GetFullPath(options.OutputPath ?? GetDefaultOutputPath(inputPath));
            if (string.Equals(inputPath, outputPath, StringComparison.OrdinalIgnoreCase))
            {
                Console.Error.WriteLine("RavenDoc output must be different from its input.");
                return 1;
            }

            if (inputPath.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
            {
                GenerateFromAssembly(
                    inputPath,
                    outputPath,
                    options.TargetFramework,
                    options.SiteOptions);
            }
            else
            {
                GenerateFromSource(
                    inputPath,
                    outputPath,
                    options.TargetFramework,
                    options.SiteOptions,
                    options.ReferencePaths);
            }

            Console.WriteLine($"RavenDoc wrote {outputPath}");
            return 0;
        }
        catch (Exception exception)
        {
            Console.Error.WriteLine($"RavenDoc failed: {exception}");
            return 1;
        }
    }

    private static void GenerateFromSource(
        string inputPath,
        string outputPath,
        string targetFramework,
        DocumentationSiteOptions siteOptions,
        IReadOnlyList<string> referencePaths)
    {
        var workspace = RavenWorkspace.Create(targetFramework: targetFramework);
        workspace.Services.SyntaxTreeProvider.ParseOptions = new ParseOptions
        {
            DocumentationMode = true,
            DocumentationFormat = DocumentationFormat.Markdown
        };

        Compilation compilation;
        if (IsProjectFile(inputPath))
        {
            var projectId = workspace.OpenProject(inputPath);
            compilation = workspace.GetCompilation(projectId);
        }
        else
        {
            var sourcePaths = GetSourcePaths(inputPath);
            if (sourcePaths.Length == 0)
                throw new InvalidOperationException("No Raven source files were found.");

            var assemblyName = Directory.Exists(inputPath)
                ? new DirectoryInfo(inputPath).Name
                : Path.GetFileNameWithoutExtension(inputPath);
            var parseOptions = workspace.Services.SyntaxTreeProvider.ParseOptions;
            var syntaxTrees = sourcePaths
                .Select(sourcePath => SyntaxTree.ParseText(
                    SourceText.From(File.ReadAllText(sourcePath)),
                    parseOptions,
                    sourcePath))
                .ToArray();
            var references = GetFrameworkReferencePaths(targetFramework)
                .Concat(referencePaths.Select(Path.GetFullPath))
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .Select(MetadataReference.CreateFromFile)
                .ToArray();
            compilation = Compilation.Create(
                    assemblyName,
                    options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(references)
                .AddSyntaxTreesWithLocalMacros(syntaxTrees);
        }

        ReportErrors(compilation);
        DocumentationGenerator.ProcessCompilation(compilation, outputPath, siteOptions);
    }

    private static void GenerateFromAssembly(
        string assemblyPath,
        string outputPath,
        string targetFramework,
        DocumentationSiteOptions siteOptions)
    {
        var targetReference = MetadataReference.CreateFromFile(assemblyPath);
        var references = new List<MetadataReference> { targetReference };
        var seenPaths = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { assemblyPath };

        foreach (var path in GetFrameworkReferencePaths(targetFramework)
            .Concat(Directory.EnumerateFiles(Path.GetDirectoryName(assemblyPath)!, "*.dll")))
        {
            var fullPath = Path.GetFullPath(path);
            if (seenPaths.Add(fullPath))
                references.Add(MetadataReference.CreateFromFile(fullPath));
        }

        var compilation = Compilation.Create(
                "RavenDoc.MetadataHost",
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references.ToArray());
        _ = compilation.GetDiagnostics();
        var assembly = compilation.GetAssemblyOrModuleSymbol(targetReference) as IAssemblySymbol
            ?? throw new InvalidOperationException($"Could not load assembly symbols from '{assemblyPath}'.");

        DocumentationGenerator.ProcessAssembly(compilation, assembly, outputPath, siteOptions);
    }

    private static void ReportErrors(Compilation compilation)
    {
        var errors = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();
        if (errors.Length == 0)
            return;

        foreach (var diagnostic in errors)
            Console.Error.WriteLine(diagnostic);

        throw new InvalidOperationException(
            $"Documentation input contains {errors.Length} compilation error{(errors.Length == 1 ? string.Empty : "s")}.");
    }

    private static string[] GetSourcePaths(string inputPath)
    {
        if (File.Exists(inputPath))
            return RavenFileExtensions.HasRavenExtension(inputPath) ? [inputPath] : [];

        return RavenFileExtensions.All
            .SelectMany(extension => Directory.EnumerateFiles(inputPath, $"*{extension}", SearchOption.AllDirectories))
            .Where(path => !path.Contains($"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase))
            .Where(path => !path.Contains($"{Path.DirectorySeparatorChar}bin{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();
    }

    private static IEnumerable<string> GetFrameworkReferencePaths(string targetFramework)
    {
        var version = TargetFrameworkResolver.ResolveVersion(targetFramework);
        return TargetFrameworkResolver.GetReferenceAssemblies(version);
    }

    private static bool IsProjectFile(string path)
        => path.EndsWith(".rvnproj", StringComparison.OrdinalIgnoreCase) ||
           path.EndsWith(".ravenproj", StringComparison.OrdinalIgnoreCase);

    private static string GetDefaultOutputPath(string inputPath)
    {
        var directory = Directory.Exists(inputPath)
            ? inputPath
            : Path.GetDirectoryName(inputPath) ?? Environment.CurrentDirectory;
        return Path.Combine(directory, "_site");
    }

    private static bool TryParseArguments(string[] args, out RavenDocOptions options)
    {
        string? inputPath = null;
        string? outputPath = null;
        var targetFramework = DefaultTargetFramework;
        var siteLinks = new List<DocumentationSiteLink>();
        var referencePaths = new List<string>();
        var showHelp = false;

        for (var index = 0; index < args.Length; index++)
        {
            switch (args[index])
            {
                case "-h":
                case "--help":
                    showHelp = true;
                    break;
                case "-o":
                case "--output":
                    if (!TryReadValue(args, ref index, out outputPath))
                    {
                        Console.Error.WriteLine("Missing value for --output.");
                        options = default;
                        return false;
                    }
                    break;
                case "-f":
                case "--framework":
                    if (!TryReadValue(args, ref index, out targetFramework))
                    {
                        Console.Error.WriteLine("Missing value for --framework.");
                        options = default;
                        return false;
                    }
                    break;
                case "--nav":
                    if (!TryReadValue(args, ref index, out var navigationValue) ||
                        !TryParseNavigationLink(navigationValue!, out var navigationLink))
                    {
                        Console.Error.WriteLine(
                            "Invalid --nav value. Use --nav \"Label=https://example.com/path/\".");
                        options = default;
                        return false;
                    }
                    siteLinks.Add(navigationLink);
                    break;
                case "-r":
                case "--reference":
                    if (!TryReadValue(args, ref index, out var referencePath))
                    {
                        Console.Error.WriteLine("Missing value for --reference.");
                        options = default;
                        return false;
                    }
                    referencePaths.Add(referencePath!);
                    break;
                default:
                    if (args[index].StartsWith('-'))
                    {
                        Console.Error.WriteLine($"Unknown RavenDoc option: {args[index]}");
                        options = default;
                        return false;
                    }

                    if (inputPath is not null)
                    {
                        Console.Error.WriteLine("RavenDoc accepts one project, source path, directory, or assembly.");
                        options = default;
                        return false;
                    }

                    inputPath = args[index];
                    break;
            }
        }

        if (!showHelp && string.IsNullOrWhiteSpace(inputPath))
        {
            Console.Error.WriteLine("RavenDoc requires an input path.");
            PrintHelp();
            options = default;
            return false;
        }

        options = new RavenDocOptions(
            inputPath,
            outputPath,
            targetFramework!,
            new DocumentationSiteOptions(siteLinks),
            referencePaths,
            showHelp);
        return true;
    }

    private static bool TryParseNavigationLink(
        string value,
        out DocumentationSiteLink link)
    {
        var separator = value.IndexOf('=');
        if (separator <= 0 || separator == value.Length - 1)
        {
            link = null!;
            return false;
        }

        var label = value[..separator].Trim();
        var url = value[(separator + 1)..].Trim();
        if (label.Length == 0 ||
            url.Length == 0 ||
            !Uri.TryCreate(url, UriKind.RelativeOrAbsolute, out _))
        {
            link = null!;
            return false;
        }

        link = new DocumentationSiteLink(label, url);
        return true;
    }

    private static bool TryReadValue(string[] args, ref int index, out string? value)
    {
        if (index + 1 >= args.Length || args[index + 1].StartsWith('-'))
        {
            value = null;
            return false;
        }

        value = args[++index];
        return true;
    }

    private static void PrintHelp()
    {
        Console.WriteLine("""
            RavenDoc — Raven API documentation publisher

            Usage:
              ravendoc <project.rvnproj|source.rvn|source-directory|library.dll> [options]

            Options:
              -o, --output <directory>    HTML site output (default: <input-directory>/_site)
              -f, --framework <tfm>       Target framework used for references (default: net10.0)
                  --nav <label=url>        Add a related-site link to the generated header
              -r, --reference <assembly>   Add a metadata reference for source input
              -h, --help                  Show help

            RavenDoc reads Markdown directly from Raven source symbols or from a
            compiled library with an adjacent <LibraryName>.docs sidecar.
            """);
    }

    private readonly record struct RavenDocOptions(
        string? InputPath,
        string? OutputPath,
        string TargetFramework,
        DocumentationSiteOptions SiteOptions,
        IReadOnlyList<string> ReferencePaths,
        bool ShowHelp);
}
