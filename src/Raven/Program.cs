using System.Diagnostics;
using System.Reflection;
using System.Text.Json;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.Compiler.Core;

if (args.Length == 1 && args[0] is "--version" or "version")
{
    Console.WriteLine(GetVersion());
    return 0;
}

if (args.Length == 0 || IsHelp(args[0]))
{
    PrintHelp();
    return 0;
}

if (string.Equals(args[0], "init", StringComparison.OrdinalIgnoreCase))
    return RunInitCommand(args);

if (RavenFileExtensions.HasRavenExtension(args[0]))
    return RunSourceFileCommand(args, sourceArgumentIndex: 0);

if (string.Equals(args[0], "build", StringComparison.OrdinalIgnoreCase) ||
    string.Equals(args[0], "clean", StringComparison.OrdinalIgnoreCase) ||
    string.Equals(args[0], "run", StringComparison.OrdinalIgnoreCase))
{
    if (string.Equals(args[0], "run", StringComparison.OrdinalIgnoreCase) &&
        TryFindSourceArgument(args, startIndex: 1, out var sourceArgumentIndex))
    {
        return RunSourceFileCommand(args, sourceArgumentIndex);
    }

    return RunSdkProjectCommand(args[0].ToLowerInvariant(), args);
}

if (string.Equals(args[0], "dev", StringComparison.OrdinalIgnoreCase))
    return RunDevCommand(args);

if (string.Equals(args[0], "sdk", StringComparison.OrdinalIgnoreCase))
    return RunSdkCommand(args);

if (string.Equals(args[0], "doctor", StringComparison.OrdinalIgnoreCase))
    return RunDoctorCommand(args);

Console.Error.WriteLine($"Unknown rvn command '{args[0]}'. Use rvn build/run/clean, rvn init, rvn sdk path, rvn dev, or rvnc for direct compiler-driver invocations.");
return 1;

static int RunSdkCommand(string[] args)
{
    if (args.Length == 2 && string.Equals(args[1], "path", StringComparison.OrdinalIgnoreCase))
    {
        var sdkRoot = TryFindSdkRoot();
        if (sdkRoot is null)
        {
            Console.Error.WriteLine("Unable to locate the Raven SDK. Set RAVEN_SDK_ROOT or run rvn from an installed Raven SDK.");
            return 1;
        }

        Console.WriteLine(sdkRoot);
        return 0;
    }

    Console.Error.WriteLine("Usage: rvn sdk path");
    return 1;
}

static int RunDoctorCommand(string[] args)
{
    if (args.Length != 1)
    {
        Console.Error.WriteLine("Usage: rvn doctor");
        return 1;
    }

    var hasErrors = false;
    Console.WriteLine($"Raven {GetVersion()}");

    if (TryGetDotnetSdkVersion(out var dotnetVersion, out var dotnetError))
    {
        Console.WriteLine($"[ok] .NET SDK {dotnetVersion}");
    }
    else
    {
        Console.Error.WriteLine($"[missing] .NET SDK: {dotnetError}");
        hasErrors = true;
    }

    var sdkRoot = TryFindSdkRoot();
    if (sdkRoot is null)
    {
        Console.Error.WriteLine("[missing] Raven SDK root. Set RAVEN_SDK_ROOT or install Raven and add its bin directory to PATH.");
        return 1;
    }

    Console.WriteLine($"[ok] Raven SDK {sdkRoot}");

    var requiredFiles = new[]
    {
        Path.Combine("sdk", "Raven.Core.dll"),
        Path.Combine("sdk", "build", "Raven.Language.targets"),
        Path.Combine("tools", "rvn", "rvn.dll"),
        Path.Combine("tools", "rvnc", "rvnc.dll"),
        Path.Combine("tools", "language-server", "Raven.LanguageServer.dll")
    };

    foreach (var relativePath in requiredFiles)
    {
        if (File.Exists(Path.Combine(sdkRoot, relativePath)))
        {
            Console.WriteLine($"[ok] {relativePath}");
        }
        else
        {
            Console.Error.WriteLine($"[missing] {relativePath}");
            hasErrors = true;
        }
    }

    return hasErrors ? 1 : 0;
}

static bool TryGetDotnetSdkVersion(out string version, out string error)
{
    var startInfo = new ProcessStartInfo("dotnet", "--version")
    {
        UseShellExecute = false,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        CreateNoWindow = true
    };

    try
    {
        using var process = Process.Start(startInfo);
        if (process is null)
        {
            version = string.Empty;
            error = "failed to start dotnet";
            return false;
        }

        var standardOutput = process.StandardOutput.ReadToEnd().Trim();
        var standardError = process.StandardError.ReadToEnd().Trim();
        process.WaitForExit();

        if (process.ExitCode == 0 && standardOutput.Length > 0)
        {
            version = standardOutput;
            error = string.Empty;
            return true;
        }

        version = string.Empty;
        error = standardError.Length > 0 ? standardError : $"dotnet --version exited with code {process.ExitCode}";
        return false;
    }
    catch (Exception ex) when (ex is InvalidOperationException or System.ComponentModel.Win32Exception)
    {
        version = string.Empty;
        error = ex.Message;
        return false;
    }
}

static string? TryFindSdkRoot()
{
    var configuredRoot = Environment.GetEnvironmentVariable("RAVEN_SDK_ROOT");
    if (!string.IsNullOrWhiteSpace(configuredRoot))
    {
        var fullPath = Path.GetFullPath(configuredRoot);
        if (IsSdkRoot(fullPath))
            return fullPath;
    }

    var directory = new DirectoryInfo(AppContext.BaseDirectory);
    while (directory is not null)
    {
        if (IsSdkRoot(directory.FullName))
            return directory.FullName;

        directory = directory.Parent;
    }

    return null;
}

static bool IsSdkRoot(string path)
    => File.Exists(Path.Combine(path, "VERSION")) &&
       File.Exists(Path.Combine(path, "sdk", "build", "Raven.Language.targets"));

static string GetVersion()
    => Assembly.GetEntryAssembly()?
           .GetCustomAttribute<AssemblyInformationalVersionAttribute>()?
           .InformationalVersion ?? "unknown";

static bool TryFindSourceArgument(string[] args, int startIndex, out int sourceArgumentIndex)
{
    var separatorIndex = Array.IndexOf(args, "--", startIndex);
    var endIndex = separatorIndex >= 0 ? separatorIndex : args.Length;

    for (var i = startIndex; i < endIndex; i++)
    {
        if (RavenFileExtensions.HasRavenExtension(args[i]))
        {
            sourceArgumentIndex = i;
            return true;
        }
    }

    sourceArgumentIndex = -1;
    return false;
}

static int RunSourceFileCommand(string[] args, int sourceArgumentIndex)
{
    var sourcePath = Path.GetFullPath(args[sourceArgumentIndex]);
    if (!File.Exists(sourcePath))
    {
        Console.Error.WriteLine($"Raven source file '{sourcePath}' does not exist.");
        return 1;
    }

    var compilerPath = TryFindCompilerDriverPath();
    if (compilerPath is null)
    {
        Console.Error.WriteLine("Unable to locate the Raven compiler driver (rvnc). Reinstall the Raven SDK or build src/Raven.Compiler.");
        return 1;
    }

    var separatorIndex = Array.IndexOf(args, "--");
    var toolArguments = new List<string>();
    const int toolStartIndex = 1;
    var toolEndIndex = sourceArgumentIndex == 0
        ? 1
        : separatorIndex >= 0 ? separatorIndex : args.Length;

    for (var i = toolStartIndex; i < toolEndIndex; i++)
    {
        if (i == sourceArgumentIndex)
            continue;

        if (RavenFileExtensions.HasRavenExtension(args[i]))
        {
            Console.Error.WriteLine("File-based applications accept exactly one Raven source file.");
            return 1;
        }

        toolArguments.Add(args[i]);
    }

    var applicationArguments = sourceArgumentIndex == 0
        ? args.Skip(args.Length > 1 && args[1] == "--" ? 2 : 1).ToArray()
        : separatorIndex >= 0
            ? args.Skip(separatorIndex + 1).ToArray()
            : [];
    var outputDirectory = Path.Combine(Path.GetTempPath(), "raven", "run", Guid.NewGuid().ToString("N"));
    var outputPath = Path.Combine(outputDirectory, $"{Path.GetFileNameWithoutExtension(sourcePath)}.dll");
    Directory.CreateDirectory(outputDirectory);

    try
    {
        var startInfo = new ProcessStartInfo("dotnet")
        {
            UseShellExecute = false
        };
        startInfo.Environment["RAVEN_FRONTEND_INVOCATION"] = "1";
        startInfo.ArgumentList.Add(compilerPath);
        foreach (var argument in toolArguments)
            startInfo.ArgumentList.Add(argument);
        startInfo.ArgumentList.Add("--run");
        startInfo.ArgumentList.Add("--output");
        startInfo.ArgumentList.Add(outputPath);
        startInfo.ArgumentList.Add(sourcePath);

        if (applicationArguments.Length > 0)
        {
            startInfo.ArgumentList.Add("--");
            foreach (var argument in applicationArguments)
                startInfo.ArgumentList.Add(argument);
        }

        using var process = Process.Start(startInfo);
        if (process is null)
        {
            Console.Error.WriteLine("Failed to start the Raven compiler.");
            return 1;
        }

        process.WaitForExit();
        return process.ExitCode;
    }
    catch (Exception ex) when (ex is InvalidOperationException or System.ComponentModel.Win32Exception)
    {
        Console.Error.WriteLine($"Failed to start the Raven compiler: {ex.Message}");
        return 1;
    }
    finally
    {
        try
        {
            Directory.Delete(outputDirectory, recursive: true);
        }
        catch (IOException)
        {
        }
        catch (UnauthorizedAccessException)
        {
        }
    }
}

static string? TryFindCompilerDriverPath()
{
    if (TryFindSdkRoot() is { } sdkRoot)
    {
        var installedCompilerPath = Path.Combine(sdkRoot, "tools", "rvnc", "rvnc.dll");
        if (File.Exists(installedCompilerPath))
            return installedCompilerPath;
    }

    var baseDirectory = new DirectoryInfo(AppContext.BaseDirectory);
    var targetFramework = baseDirectory.Name;
    var configuration = baseDirectory.Parent?.Name;
    if (configuration is not null)
    {
        var developmentCompilerPath = Path.GetFullPath(Path.Combine(
            AppContext.BaseDirectory,
            "..",
            "..",
            "..",
            "..",
            "Raven.Compiler",
            "bin",
            configuration,
            targetFramework,
            "rvnc.dll"));
        if (File.Exists(developmentCompilerPath))
            return developmentCompilerPath;
    }

    var adjacentCompilerPath = Path.Combine(AppContext.BaseDirectory, "rvnc.dll");
    return File.Exists(adjacentCompilerPath) ? adjacentCompilerPath : null;
}

static int RunSdkProjectCommand(string commandName, string[] args)
{
    if (args.Length > 1 && IsHelp(args[1]))
    {
        PrintSdkProjectCommandHelp(commandName);
        return 0;
    }

    if (!TryParseSdkProjectCommand(args, out var projectFilePath, out var forwardedArgs))
        return 1;

    var dotnetArgs = new List<string> { commandName };
    if (commandName == "run")
    {
        dotnetArgs.Add("--project");
        dotnetArgs.Add(projectFilePath);
    }
    else
    {
        dotnetArgs.Add(projectFilePath);
    }

    dotnetArgs.AddRange(forwardedArgs);
    return RunDotnet(dotnetArgs);
}

static bool TryParseSdkProjectCommand(
    string[] args,
    out string projectFilePath,
    out IReadOnlyList<string> forwardedArgs)
{
    string? project = null;
    var rest = new List<string>();

    for (var i = 1; i < args.Length; i++)
    {
        var arg = args[i];
        if (project is null && !arg.StartsWith('-') && IsRavenProjectPath(arg))
        {
            project = arg;
            continue;
        }

        rest.Add(arg);
    }

    project ??= TryFindDefaultProjectFile();
    if (project is null)
    {
        Console.Error.WriteLine("No Raven project file was specified and the current directory does not contain exactly one .rvnproj file.");
        Console.Error.WriteLine("Usage: rvn build [project.rvnproj] [dotnet-build-options]");
        Console.Error.WriteLine("       rvn run [project.rvnproj] [dotnet-run-options] [-- application-args]");
        Console.Error.WriteLine("       rvn clean [project.rvnproj] [dotnet-clean-options]");
        projectFilePath = string.Empty;
        forwardedArgs = [];
        return false;
    }

    projectFilePath = Path.GetFullPath(project);
    forwardedArgs = rest;
    return true;
}

static bool IsRavenProjectPath(string path)
{
    var extension = Path.GetExtension(path);
    return string.Equals(extension, RavenFileExtensions.Project, StringComparison.OrdinalIgnoreCase) ||
        string.Equals(extension, RavenFileExtensions.LegacyProject, StringComparison.OrdinalIgnoreCase);
}

static string? TryFindDefaultProjectFile()
{
    var currentDirectory = Directory.GetCurrentDirectory();
    var projects = Directory.GetFiles(currentDirectory, $"*{RavenFileExtensions.Project}");
    if (projects.Length == 1)
        return projects[0];

    if (projects.Length > 1)
    {
        Console.Error.WriteLine("Multiple .rvnproj files were found. Specify the project explicitly.");
        foreach (var project in projects.OrderBy(static p => p, StringComparer.OrdinalIgnoreCase))
            Console.Error.WriteLine($"- {Path.GetFileName(project)}");
        return null;
    }

    var legacyProjects = Directory.GetFiles(currentDirectory, $"*{RavenFileExtensions.LegacyProject}");
    return legacyProjects.Length == 1 ? legacyProjects[0] : null;
}

static int RunDotnet(IReadOnlyList<string> dotnetArgs)
{
    var startInfo = new ProcessStartInfo("dotnet")
    {
        UseShellExecute = false
    };

    foreach (var arg in dotnetArgs)
        startInfo.ArgumentList.Add(arg);

    try
    {
        using var process = Process.Start(startInfo);
        if (process is null)
        {
            Console.Error.WriteLine("Failed to start dotnet.");
            return 1;
        }

        process.WaitForExit();
        return process.ExitCode;
    }
    catch (Exception ex) when (ex is InvalidOperationException or System.ComponentModel.Win32Exception)
    {
        Console.Error.WriteLine($"Failed to start dotnet: {ex.Message}");
        return 1;
    }
}

static int RunDevCommand(string[] args)
{
    if (!TryParseDevCommand(args, out var options))
        return 1;

    if (options.Command == DevCommand.ParseSequence)
        SyntaxParserFlags.PrintParseSequence = true;

    CompilerWorkspaceSession session;
    try
    {
        session = CompilerWorkspaceFactory.Create(new CompilerWorkspaceOptions
        {
            InputPaths = options.InputPaths,
            TargetFramework = options.TargetFramework,
            AdditionalReferences = options.References,
            RestoreProjectReferences = !options.NoProjectRestore,
            EmbedCoreTypes = true
        });
    }
    catch (Exception ex) when (ex is ArgumentException or FileNotFoundException or InvalidOperationException)
    {
        Console.Error.WriteLine(ex.Message);
        return 1;
    }

    var project = session.Project;
    var document = SelectDocument(project, options.InputPaths, options.DocumentPath);
    if (document is not null && options.SourceTextPath is not null)
    {
        using var sourceStream = File.OpenRead(options.SourceTextPath);
        var updatedDocument = document.WithText(SourceText.From(sourceStream));
        session.Workspace.TryApplyChanges(updatedDocument.Project.Solution);
        project = session.Workspace.CurrentSolution.GetProject(session.ProjectId)!;
        document = project.GetDocument(document.Id);
    }

    var compilation = session.Workspace.GetCompilation(session.ProjectId);
    var diagnostics = session.Workspace.GetDiagnostics(session.ProjectId);
    var syntaxTree = document?.GetSyntaxTreeAsync().GetAwaiter().GetResult();
    var root = syntaxTree?.GetRoot();

    switch (options.Command)
    {
        case DevCommand.Syntax:
            if (root is null)
                return SingleDocumentRequired(options.CommandName);

            var syntaxRoot = options.SyntaxView == SyntaxView.Expanded && document is not null
                ? document.GetExpandedSyntaxRootAsync().GetAwaiter().GetResult() ?? root
                : root;

            if (options.SyntaxTreeFormat == SyntaxTreeFormat.Json)
            {
                Console.WriteLine(JsonSerializer.Serialize(
                    new SyntaxVisualizerDocument(
                        options.SyntaxView == SyntaxView.Expanded ? "expanded" : "authored",
                        syntaxRoot.ToFullString(),
                        CreateSyntaxVisualizerNode(syntaxRoot, propertyName: null)),
                    new JsonSerializerOptions
                    {
                        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                        WriteIndented = true
                    }));
                return 0;
            }

            syntaxRoot.PrintSyntaxTree(new PrinterOptions
            {
                IncludeNames = true,
                IncludeTokens = true,
                IncludeTrivia = true,
                IncludeSpans = true,
                IncludeLocations = true,
                Colorize = true,
                ExpandListsAsProperties = options.SyntaxTreeFormat == SyntaxTreeFormat.Flat,
                IncludeDiagnostics = true,
                IncludeAnnotations = true,
                DiagnosticsAsChildren = true,
                AnnotationsAsChildren = true
            });
            return 0;

        case DevCommand.SyntaxInternal:
            if (root is null)
                return SingleDocumentRequired(options.CommandName);

            var green = (GreenNode?)typeof(SyntaxNode)
                .GetField("Green", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)!
                .GetValue(root);
            Console.WriteLine(Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinter.PrintToString(
                green!,
                new Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinterOptions
                {
                    IncludeTrivia = true,
                    IncludeTriviaText = true,
                    FlattenSyntaxLists = false,
                    IncludeSlotIndices = true,
                    IncludeWidths = true,
                    HideZeroWidthSlots = false,
                    IncludeDiagnostics = true,
                    IncludeAnnotations = true,
                    DiagnosticsAsChildren = true,
                    AnnotationsAsChildren = true
                }));
            return 0;

        case DevCommand.Dump:
            if (root is null)
                return SingleDocumentRequired(options.CommandName);

            PrintSyntaxDump(root, compilation, syntaxTree, diagnostics, options.DumpFormat, includeDiagnostics: true);
            return 0;

        case DevCommand.Macros:
            if (root is null || syntaxTree is null || document is null)
                return SingleDocumentRequired(options.CommandName);

            var expandedRoot = document.GetExpandedSyntaxRootAsync().GetAwaiter().GetResult() ?? root;
            if (options.MacroMode is MacroDumpMode.Original or MacroDumpMode.Both)
            {
                Console.WriteLine("=== Original ===");
                PrintSyntaxDump(root, compilation, syntaxTree, diagnostics, options.DumpFormat, includeDiagnostics: true);
            }

            if (options.MacroMode is MacroDumpMode.Expanded or MacroDumpMode.Both)
            {
                Console.WriteLine("=== Expanded ===");
                PrintSyntaxDump(expandedRoot, compilation, syntaxTree: null, diagnostics, options.DumpFormat, includeDiagnostics: false);
            }
            return 0;

        case DevCommand.Binders:
            if (syntaxTree is null)
                return SingleDocumentRequired(options.CommandName);

            compilation.GetSemanticModel(syntaxTree).PrintBinderTree();
            return 0;

        case DevCommand.BoundTree:
            if (syntaxTree is null)
                return SingleDocumentRequired(options.CommandName);

            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            semanticModel.PrintBinderTree();
            Console.WriteLine();
            semanticModel.PrintBoundTree(
                includeChildPropertyNames: true,
                groupChildCollections: true,
                displayCollectionIndices: false,
                onlyBlockRoots: false,
                includeErrorNodes: true,
                view: options.BoundTreeView);
            return 0;

        case DevCommand.Symbols:
            PrintSymbols(compilation.Assembly, options.SymbolMode);
            return 0;

        case DevCommand.Quote:
            if (compilation.SyntaxTrees.FirstOrDefault() is not { } firstSyntaxTree)
                return SingleDocumentRequired(options.CommandName);

            var quoted = RavenQuoter.QuoteText(firstSyntaxTree.GetText()!.ToString(), new RavenQuoterOptions
            {
                IncludeTrivia = true,
                GenerateUsingDirectives = true,
                UseStaticSyntaxFactoryImport = true,
                UseNamedArguments = true,
                IgnoreNullValue = true,
                UseFactoryPropsForSimpleTokens = true
            });
            Console.WriteLine(quoted);
            return 0;

        case DevCommand.ParseSequence:
            return 0;

        default:
            Console.Error.WriteLine($"Dev command '{options.CommandName}' is not implemented yet.");
            return 1;
    }
}

static int SingleDocumentRequired(string commandName)
{
    Console.Error.WriteLine($"rvn dev {commandName} requires a single source document.");
    return 1;
}

static Document? SelectDocument(
    Project project,
    IReadOnlyList<string> inputPaths,
    string? documentPath)
{
    if (documentPath is not null)
    {
        var fullDocumentPath = Path.GetFullPath(documentPath);
        return project.Documents.FirstOrDefault(
            document => document.FilePath is not null &&
                string.Equals(Path.GetFullPath(document.FilePath), fullDocumentPath, StringComparison.OrdinalIgnoreCase));
    }

    var sourceInputs = inputPaths
        .Where(static path => !RavenFileExtensions.HasProjectExtension(path))
        .Select(Path.GetFullPath)
        .ToArray();

    if (sourceInputs.Length == 1)
    {
        var sourceInput = sourceInputs[0];
        var matchingDocument = project.Documents.FirstOrDefault(
            document => document.FilePath is not null &&
                string.Equals(Path.GetFullPath(document.FilePath), sourceInput, StringComparison.OrdinalIgnoreCase));
        if (matchingDocument is not null)
            return matchingDocument;
    }

    var userDocuments = project.Documents
        .Where(static document => document.FilePath is null || !Path.GetFileName(document.FilePath).EndsWith(".g.rvn", StringComparison.OrdinalIgnoreCase))
        .ToArray();

    return userDocuments.Length == 1 ? userDocuments[0] : null;
}

static void PrintSyntaxDump(
    SyntaxNode root,
    Compilation compilation,
    SyntaxTree? syntaxTree,
    IReadOnlyList<Diagnostic> diagnostics,
    DevDumpFormat dumpFormat,
    bool includeDiagnostics)
{
    if (dumpFormat == DevDumpFormat.Plain)
    {
        Console.WriteLine(root.ToFullString());
        return;
    }

    ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
    var prettyDiagnostics = includeDiagnostics && syntaxTree is not null
        ? diagnostics.Where(d => d.Location.SourceTree == syntaxTree)
        : null;
    Console.WriteLine(root.WriteNodeToText(
        compilation,
        includeDiagnostics: includeDiagnostics && syntaxTree is not null,
        diagnostics: prettyDiagnostics));
}

static bool TryParseDevCommand(string[] args, out DevCommandOptions options)
{
    options = DevCommandOptions.Empty;
    if (args.Length < 2 || IsHelp(args[1]))
    {
        PrintDevHelp();
        return false;
    }

    var commandName = args[1];
    var command = commandName.ToLowerInvariant() switch
    {
        "syntax" or "syntax-tree" => DevCommand.Syntax,
        "syntax-internal" or "green-tree" => DevCommand.SyntaxInternal,
        "dump" => DevCommand.Dump,
        "macros" or "macro-source" => DevCommand.Macros,
        "binders" => DevCommand.Binders,
        "bound-tree" => DevCommand.BoundTree,
        "parse-sequence" => DevCommand.ParseSequence,
        "symbols" => DevCommand.Symbols,
        "quote" => DevCommand.Quote,
        _ => DevCommand.Unknown
    };

    if (command == DevCommand.Unknown)
    {
        Console.Error.WriteLine($"Unknown dev command '{args[1]}'.");
        PrintDevHelp();
        return false;
    }

    string? targetFramework = null;
    var references = new List<string>();
    var inputs = new List<string>();
    var noProjectRestore = false;
    var syntaxTreeFormat = SyntaxTreeFormat.Flat;
    var syntaxView = SyntaxView.Authored;
    string? documentPath = null;
    string? sourceTextPath = null;
    var dumpFormat = DevDumpFormat.Pretty;
    var macroMode = MacroDumpMode.Both;
    var boundTreeView = BoundTreeView.Original;
    var symbolMode = SymbolDumpMode.List;

    for (var i = 2; i < args.Length; i++)
    {
        var arg = args[i];
        switch (arg)
        {
            case "--framework":
                if (!TryConsumeValue(args, ref i, out targetFramework))
                    return MissingValue(arg);
                break;
            case "--ref":
            case "--refs":
                if (!TryConsumeValue(args, ref i, out var reference))
                    return MissingValue(arg);
                references.Add(reference);
                break;
            case "--no-project-restore":
                noProjectRestore = true;
                break;
            case "--syntax-view":
                if (!TryConsumeValue(args, ref i, out var syntaxViewValue) ||
                    !TryParseSyntaxView(syntaxViewValue, out syntaxView))
                    return InvalidValue(arg);
                break;
            case "--document":
                if (!TryConsumeValue(args, ref i, out documentPath))
                    return MissingValue(arg);
                break;
            case "--source-text":
                if (!TryConsumeValue(args, ref i, out sourceTextPath))
                    return MissingValue(arg);
                break;
            case "--bound-tree-view":
            case "--bt-view":
                if (!TryConsumeValue(args, ref i, out var view) || !TryParseBoundTreeView(view, out boundTreeView))
                    return InvalidValue(arg);
                break;
            default:
                if (arg.StartsWith('-'))
                {
                    Console.Error.WriteLine($"Unknown dev option '{arg}'.");
                    return false;
                }

                if (command == DevCommand.Syntax && TryParseSyntaxTreeFormat(arg, out var parsedSyntaxTreeFormat))
                {
                    syntaxTreeFormat = parsedSyntaxTreeFormat;
                    break;
                }

                if ((command == DevCommand.Dump || command == DevCommand.Macros) &&
                    TryParseDumpFormat(arg, out var parsedDumpFormat))
                {
                    dumpFormat = parsedDumpFormat;
                    break;
                }

                if (command == DevCommand.Macros && TryParseMacroMode(arg, out var parsedMacroMode))
                {
                    macroMode = parsedMacroMode;
                    break;
                }

                if (command == DevCommand.Symbols && TryParseSymbolDumpMode(arg, out var parsedSymbolMode))
                {
                    symbolMode = parsedSymbolMode;
                    break;
                }

                if (command == DevCommand.BoundTree && TryParseBoundTreeView(arg, out var parsedBoundTreeView))
                {
                    boundTreeView = parsedBoundTreeView;
                    break;
                }

                inputs.Add(arg);
                break;
        }
    }

    if (inputs.Count == 0)
    {
        Console.Error.WriteLine($"rvn dev {commandName} requires a source or project input.");
        return false;
    }

    options = new DevCommandOptions(
        commandName,
        command,
        inputs,
        references,
        targetFramework,
        noProjectRestore,
        syntaxTreeFormat,
        syntaxView,
        documentPath,
        sourceTextPath,
        dumpFormat,
        macroMode,
        boundTreeView,
        symbolMode);
    return true;
}

static bool MissingValue(string option)
{
    Console.Error.WriteLine($"Missing value for {option}.");
    return false;
}

static bool InvalidValue(string option)
{
    Console.Error.WriteLine($"Invalid value for {option}.");
    return false;
}

static bool TryConsumeValue(string[] args, ref int index, out string value)
{
    if (index + 1 < args.Length && !args[index + 1].StartsWith('-'))
    {
        value = args[++index];
        return true;
    }

    value = string.Empty;
    return false;
}

static bool TryParseSyntaxTreeFormat(string value, out SyntaxTreeFormat format)
{
    var normalizedValue = value.ToLowerInvariant();
    format = normalizedValue switch
    {
        "flat" => SyntaxTreeFormat.Flat,
        "group" or "grouped" => SyntaxTreeFormat.Group,
        "json" => SyntaxTreeFormat.Json,
        _ => SyntaxTreeFormat.Flat
    };
    return normalizedValue is "flat" or "group" or "grouped" or "json";
}

static bool TryParseSyntaxView(string value, out SyntaxView view)
{
    var normalizedValue = value.ToLowerInvariant();
    view = normalizedValue switch
    {
        "authored" or "original" => SyntaxView.Authored,
        "expanded" => SyntaxView.Expanded,
        _ => SyntaxView.Authored
    };
    return normalizedValue is "authored" or "original" or "expanded";
}

static SyntaxVisualizerElement CreateSyntaxVisualizerNode(SyntaxNode node, string? propertyName)
{
    var children = new List<SyntaxVisualizerElement>();
    foreach (var child in node.ChildNodesAndTokens())
    {
        if (child.TryGetNode(out var childNode))
        {
            var childPropertyName = node.GetPropertyNameForChild(childNode)
                ?? node.GetPropertyNameForListItem(childNode);
            children.Add(CreateSyntaxVisualizerNode(childNode, childPropertyName));
        }
        else if (child.TryGetToken(out var token))
        {
            var childPropertyName = node.GetPropertyNameForChild(token)
                ?? node.GetPropertyNameForListItem(token);
            children.Add(CreateSyntaxVisualizerToken(token, childPropertyName));
        }
    }

    return new SyntaxVisualizerElement(
        Category: "node",
        Kind: node.Kind.ToString(),
        RawKind: (int)node.Kind,
        PropertyName: propertyName,
        Text: null,
        Span: CreateSyntaxVisualizerSpan(node.Span),
        FullSpan: CreateSyntaxVisualizerSpan(node.FullSpan),
        IsMissing: node.IsMissing,
        Diagnostics: CreateSyntaxVisualizerDiagnostics(node.GetDiagnostics(traverse: false)),
        Children: children);
}

static SyntaxVisualizerElement CreateSyntaxVisualizerToken(SyntaxToken token, string? propertyName)
{
    var children = new List<SyntaxVisualizerElement>();
    children.AddRange(token.LeadingTrivia.Select(trivia => CreateSyntaxVisualizerTrivia(trivia, "LeadingTrivia")));
    children.AddRange(token.TrailingTrivia.Select(trivia => CreateSyntaxVisualizerTrivia(trivia, "TrailingTrivia")));

    return new SyntaxVisualizerElement(
        Category: "token",
        Kind: token.Kind.ToString(),
        RawKind: token.RawKind,
        PropertyName: propertyName,
        Text: token.Text,
        Span: CreateSyntaxVisualizerSpan(token.Span),
        FullSpan: CreateSyntaxVisualizerSpan(token.FullSpan),
        IsMissing: token.IsMissing,
        Diagnostics: CreateSyntaxVisualizerDiagnostics(token.GetDiagnostics()),
        Children: children);
}

static SyntaxVisualizerElement CreateSyntaxVisualizerTrivia(SyntaxTrivia trivia, string propertyName)
{
    var structure = trivia.GetStructure();
    var children = structure is null
        ? []
        : new[] { CreateSyntaxVisualizerNode(structure, "Structure") };

    return new SyntaxVisualizerElement(
        Category: "trivia",
        Kind: trivia.Kind.ToString(),
        RawKind: (int)trivia.Kind,
        PropertyName: propertyName,
        Text: trivia.Text,
        Span: CreateSyntaxVisualizerSpan(trivia.Span),
        FullSpan: CreateSyntaxVisualizerSpan(trivia.FullSpan),
        IsMissing: false,
        Diagnostics: CreateSyntaxVisualizerDiagnostics(trivia.GetDiagnostics()),
        Children: children);
}

static SyntaxVisualizerSpan CreateSyntaxVisualizerSpan(TextSpan span)
    => new(span.Start, span.Length);

static IReadOnlyList<SyntaxVisualizerDiagnostic> CreateSyntaxVisualizerDiagnostics(IEnumerable<Diagnostic> diagnostics)
    => diagnostics
        .Select(static diagnostic => new SyntaxVisualizerDiagnostic(
            diagnostic.Id,
            diagnostic.Severity.ToString(),
            diagnostic.GetMessage()))
        .ToArray();

static bool TryParseDumpFormat(string value, out DevDumpFormat format)
{
    format = value.ToLowerInvariant() switch
    {
        "plain" => DevDumpFormat.Plain,
        "pretty" => DevDumpFormat.Pretty,
        _ => DevDumpFormat.Pretty
    };
    return value is "plain" or "pretty";
}

static bool TryParseMacroMode(string value, out MacroDumpMode mode)
{
    mode = value.ToLowerInvariant() switch
    {
        "original" => MacroDumpMode.Original,
        "expanded" => MacroDumpMode.Expanded,
        "both" => MacroDumpMode.Both,
        _ => MacroDumpMode.Both
    };
    return value is "original" or "expanded" or "both";
}

static bool TryParseBoundTreeView(string value, out BoundTreeView view)
{
    view = value.ToLowerInvariant() switch
    {
        "original" => BoundTreeView.Original,
        "lowered" => BoundTreeView.Lowered,
        "both" => BoundTreeView.Both,
        _ => BoundTreeView.Original
    };
    return value is "original" or "lowered" or "both";
}

static bool TryParseSymbolDumpMode(string value, out SymbolDumpMode mode)
{
    mode = value.ToLowerInvariant() switch
    {
        "list" => SymbolDumpMode.List,
        "hierarchy" => SymbolDumpMode.Hierarchy,
        _ => SymbolDumpMode.List
    };
    return value is "list" or "hierarchy";
}

static void PrintSymbols(IAssemblySymbol assembly, SymbolDumpMode mode)
{
    if (mode == SymbolDumpMode.Hierarchy)
    {
        PrintSymbolHierarchy(assembly.GlobalNamespace, 0);
        return;
    }

    foreach (var symbol in EnumerateSymbols(assembly.GlobalNamespace))
        Console.WriteLine($"{symbol.Kind}: {symbol.ToDisplayString()}");
}

static void PrintSymbolHierarchy(ISymbol symbol, int depth)
{
    Console.WriteLine($"{new string(' ', depth * 2)}{symbol.Kind}: {symbol.ToDisplayString()}");
    if (symbol is INamespaceOrTypeSymbol container)
    {
        foreach (var member in container.GetMembers().OrderBy(static s => s.Name, StringComparer.Ordinal))
            PrintSymbolHierarchy(member, depth + 1);
    }
}

static IEnumerable<ISymbol> EnumerateSymbols(ISymbol symbol)
{
    yield return symbol;
    if (symbol is not INamespaceOrTypeSymbol container)
        yield break;

    foreach (var member in container.GetMembers().OrderBy(static s => s.Name, StringComparer.Ordinal))
    {
        foreach (var child in EnumerateSymbols(member))
            yield return child;
    }
}

static int RunInitCommand(string[] args)
{
    string? name = null;
    var framework = TargetFrameworkUtil.GetLatestFramework();
    var isClassLibrary = false;
    var force = false;
    var typeSpecified = false;

    for (var i = 1; i < args.Length; i++)
    {
        switch (args[i])
        {
            case "-h":
            case "--help":
                PrintInitHelp();
                return 0;
            case "--name":
                if (i + 1 >= args.Length)
                {
                    Console.Error.WriteLine("Missing value for --name.");
                    PrintInitHelp();
                    return 1;
                }

                name = args[++i];
                break;
            case "--framework":
                if (i + 1 >= args.Length)
                {
                    Console.Error.WriteLine("Missing value for --framework.");
                    PrintInitHelp();
                    return 1;
                }

                framework = args[++i];
                break;
            case "--type":
                if (i + 1 >= args.Length)
                {
                    Console.Error.WriteLine("Missing value for --type.");
                    PrintInitHelp();
                    return 1;
                }

                var typeValue = args[++i];
                if (!TryParseInitProjectType(typeValue, out isClassLibrary))
                {
                    Console.Error.WriteLine($"Invalid --type '{typeValue}'. Use 'console' or 'classlib'.");
                    PrintInitHelp();
                    return 1;
                }

                typeSpecified = true;
                break;
            case "--force":
                force = true;
                break;
            default:
                if (!args[i].StartsWith('-') && !typeSpecified && TryParseInitProjectType(args[i], out isClassLibrary))
                {
                    typeSpecified = true;
                    break;
                }

                Console.Error.WriteLine($"Unknown init option '{args[i]}'.");
                PrintInitHelp();
                return 1;
        }
    }

    var cwd = Directory.GetCurrentDirectory();
    var fallbackName = Path.GetFileName(cwd.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar));
    var projectName = SanitizeProjectName(string.IsNullOrWhiteSpace(name) ? fallbackName : name!);
    var projectFilePath = Path.Combine(cwd, $"{projectName}{RavenFileExtensions.Project}");
    var srcDir = Path.Combine(cwd, "src");
    var mainSourcePath = Path.Combine(srcDir, $"main{RavenFileExtensions.Raven}");
    var binDir = Path.Combine(cwd, "bin");
    var binGitkeep = Path.Combine(binDir, ".gitkeep");
    if (!force)
    {
        var existing = new[] { projectFilePath, mainSourcePath, binGitkeep }.Where(File.Exists).ToArray();
        if (existing.Length > 0)
        {
            Console.Error.WriteLine("Init aborted: one or more scaffold files already exist.");
            foreach (var file in existing)
                Console.Error.WriteLine($"- {file}");
            Console.Error.WriteLine("Use --force to overwrite scaffold files.");
            return 1;
        }
    }

    Directory.CreateDirectory(srcDir);
    Directory.CreateDirectory(binDir);

    var outputType = isClassLibrary ? "Library" : "Exe";
    var projectXml = $"""
                      <Project Sdk="Microsoft.NET.Sdk">
                        <PropertyGroup>
                          <TargetFramework>{framework}</TargetFramework>
                          <AssemblyName>{projectName}</AssemblyName>
                          <OutputType>{outputType}</OutputType>
                        </PropertyGroup>
                        <ItemGroup>
                          <RavenCompile Include="src/**/*{RavenFileExtensions.Raven}" />
                        </ItemGroup>
                      </Project>
                      """;
    File.WriteAllText(projectFilePath, projectXml + Environment.NewLine);

    var sourceText = """
                     val message = "Hello from Raven"
                     System.Console.WriteLine(message)
                     """;
    File.WriteAllText(mainSourcePath, sourceText + Environment.NewLine);

    if (!File.Exists(binGitkeep))
        File.WriteAllText(binGitkeep, string.Empty);

    Console.WriteLine("Raven project scaffold created.");
    Console.WriteLine($"- {projectFilePath}");
    Console.WriteLine($"- {mainSourcePath}");
    Console.WriteLine($"- {binGitkeep}");
    Console.WriteLine($"Build with: dotnet build {Path.GetFileName(projectFilePath)}");
    Console.WriteLine($"Run with: dotnet run --project {Path.GetFileName(projectFilePath)}");

    return 0;
}

static void PrintHelp()
{
    Console.WriteLine("Usage: rvn <command> [options]");
    Console.WriteLine("       rvn <file.rvn> [application-args]");
    Console.WriteLine();
    Console.WriteLine("Commands:");
    Console.WriteLine("  init              Create a .rvnproj project scaffold in the current directory.");
    Console.WriteLine("  build             Build a Raven project through dotnet build.");
    Console.WriteLine("  run               Run a Raven source file or project.");
    Console.WriteLine("  clean             Clean a Raven project through dotnet clean.");
    Console.WriteLine("  dev               Run internal compiler debug views.");
    Console.WriteLine("  sdk path          Print the root of the active Raven SDK.");
    Console.WriteLine("  doctor            Check the .NET SDK and Raven installation.");
    Console.WriteLine("  --version         Print the Raven version.");
    Console.WriteLine();
    Console.WriteLine("Use 'rvn run <file.rvn>' or 'rvn <file.rvn>' for a file-based application.");
    Console.WriteLine("Project build/run/clean commands are conveniences over the .NET SDK workflow.");
    Console.WriteLine("Use rvnc for direct compiler-driver invocations.");
}

static void PrintSdkProjectCommandHelp(string commandName)
{
    switch (commandName)
    {
        case "build":
            Console.WriteLine("Usage: rvn build [project.rvnproj] [dotnet-build-options]");
            Console.WriteLine("Runs: dotnet build <project.rvnproj> [dotnet-build-options]");
            break;
        case "run":
            Console.WriteLine("Usage: rvn run <file.rvn> [compiler-options] [-- application-args]");
            Console.WriteLine("       rvn run [project.rvnproj] [dotnet-run-options] [-- application-args]");
            Console.WriteLine("Runs a source file as an isolated file-based application, or runs a project through dotnet run.");
            Console.WriteLine("Runs: dotnet run --project <project.rvnproj> [dotnet-run-options] [-- application-args]");
            break;
        case "clean":
            Console.WriteLine("Usage: rvn clean [project.rvnproj] [dotnet-clean-options]");
            Console.WriteLine("Runs: dotnet clean <project.rvnproj> [dotnet-clean-options]");
            break;
    }

    Console.WriteLine();
    Console.WriteLine("When no input is specified, rvn uses the single .rvnproj in the current directory.");
}

static void PrintDevHelp()
{
    Console.WriteLine("Usage: rvn dev <command> [options] <source-files|project-file.rvnproj>");
    Console.WriteLine();
    Console.WriteLine("Commands:");
    Console.WriteLine("  syntax [flat|group|json]  Print syntax tree (--syntax-view authored|expanded).");
    Console.WriteLine("  syntax-internal           Print internal green syntax tree.");
    Console.WriteLine("  dump [plain|pretty]       Dump source syntax view.");
    Console.WriteLine("  macros [mode]             Dump macro source views.");
    Console.WriteLine("  binders                   Print binder tree.");
    Console.WriteLine("  bound-tree                Print bound tree.");
    Console.WriteLine("  parse-sequence            Print parser sequence.");
    Console.WriteLine("  symbols [list|hierarchy]  Inspect symbols.");
    Console.WriteLine("  quote                     Print SyntaxFactory-style tree construction code.");
}

static void PrintInitHelp()
{
    Console.WriteLine("Usage: rvn init [console|classlib] [--name <project-name>] [--framework <tfm>] [--type <console|classlib>] [--force]");
    Console.WriteLine();
    Console.WriteLine("Creates a Raven project scaffold in the current directory:");
    Console.WriteLine("  - <project-name>.rvnproj");
    Console.WriteLine("  - src/main.rvn");
    Console.WriteLine("  - bin/.gitkeep");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("  console|classlib           Select the scaffold type (default: console).");
    Console.WriteLine("  --name <project-name>      Override generated project/assembly name.");
    Console.WriteLine("  --framework <tfm>          Set TargetFramework (default: latest installed).");
    Console.WriteLine("  --type <console|classlib>  Compatibility alias for selecting the scaffold type.");
    Console.WriteLine("  --force                    Overwrite scaffold files.");
}

static bool IsHelp(string value)
    => value is "-h" or "--help" or "help";

static bool TryParseInitProjectType(string value, out bool isClassLibrary)
{
    if (string.Equals(value, "console", StringComparison.OrdinalIgnoreCase) ||
        string.Equals(value, "app", StringComparison.OrdinalIgnoreCase))
    {
        isClassLibrary = false;
        return true;
    }

    if (string.Equals(value, "classlib", StringComparison.OrdinalIgnoreCase))
    {
        isClassLibrary = true;
        return true;
    }

    isClassLibrary = false;
    return false;
}

static string SanitizeProjectName(string name)
{
    if (string.IsNullOrWhiteSpace(name))
        return "RavenApp";

    var sanitized = new string(name
        .Select(ch => char.IsLetterOrDigit(ch) || ch == '_' ? ch : '_')
        .ToArray());

    if (sanitized.Length == 0)
        sanitized = "RavenApp";

    if (!char.IsLetter(sanitized[0]) && sanitized[0] != '_')
        sanitized = "_" + sanitized;

    return sanitized;
}

sealed record DevCommandOptions(
    string CommandName,
    DevCommand Command,
    IReadOnlyList<string> InputPaths,
    IReadOnlyList<string> References,
    string? TargetFramework,
    bool NoProjectRestore,
    SyntaxTreeFormat SyntaxTreeFormat,
    SyntaxView SyntaxView,
    string? DocumentPath,
    string? SourceTextPath,
    DevDumpFormat DumpFormat,
    MacroDumpMode MacroMode,
    BoundTreeView BoundTreeView,
    SymbolDumpMode SymbolMode)
{
    public static DevCommandOptions Empty { get; } = new(
        string.Empty,
        DevCommand.Unknown,
        [],
        [],
        null,
        false,
        SyntaxTreeFormat.Flat,
        SyntaxView.Authored,
        null,
        null,
        DevDumpFormat.Pretty,
        MacroDumpMode.Both,
        BoundTreeView.Original,
        SymbolDumpMode.List);
}

enum DevCommand
{
    Unknown,
    Syntax,
    SyntaxInternal,
    Dump,
    Macros,
    Binders,
    BoundTree,
    ParseSequence,
    Symbols,
    Quote
}

enum SyntaxTreeFormat
{
    Flat,
    Group,
    Json
}

enum SyntaxView
{
    Authored,
    Expanded
}

sealed record SyntaxVisualizerDocument(
    string View,
    string SourceText,
    SyntaxVisualizerElement Root);

sealed record SyntaxVisualizerElement(
    string Category,
    string Kind,
    int RawKind,
    string? PropertyName,
    string? Text,
    SyntaxVisualizerSpan Span,
    SyntaxVisualizerSpan FullSpan,
    bool IsMissing,
    IReadOnlyList<SyntaxVisualizerDiagnostic> Diagnostics,
    IReadOnlyList<SyntaxVisualizerElement> Children);

sealed record SyntaxVisualizerSpan(int Start, int Length);

sealed record SyntaxVisualizerDiagnostic(string Id, string Severity, string Message);

enum DevDumpFormat
{
    Plain,
    Pretty
}

enum MacroDumpMode
{
    Original,
    Expanded,
    Both
}

enum SymbolDumpMode
{
    List,
    Hierarchy
}
