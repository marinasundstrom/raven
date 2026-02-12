using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection.PortableExecutable;
using System.Text;

using Raven;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Spectre.Console;

using static Raven.AppHostBuilder;
using static Raven.ConsoleEx;

var stopwatch = Stopwatch.StartNew();

// Options:
// --framework <tfm> - target framework
// --refs <path>     - additional metadata reference (repeatable)
// --raven-core <path> - path to a prebuilt Raven.Core.dll (skips embedded core types)
// --emit-core-types-only - embed the Raven.Core shims instead of referencing Raven.Core.dll
// --output-type <console|classlib> - output kind
// --unsafe           - enable unsafe mode (required for pointer declarations/usages)
// -o <path>         - output assembly path
// -s [flat|group]   - display the syntax tree (single file only)
// -d [plain|pretty[:no-diagnostics]] - dump syntax (single file only)
// -r                - print the source (single file only)
// -b                - print binder tree (single file only)
// -bt               - print binder and bound tree (single file only)
// -q                - print AST as C# compilable code
// --symbols [list|hierarchy] - inspect symbols produced from source
// --doc-tool [ravendoc|comments] - documentation generator
// --doc-format [md|xml] - documentation format (comment emission only)
// --no-emit         - skip emitting the output assembly
// --fix             - apply supported code fixes before diagnostics/emission
// --format          - normalize whitespace and indentation in source files
// --highlight       - display diagnostics with highlighted source
// --suggestions     - display instructional rewrite suggestions for diagnostics that provide them
// -h, --help        - display help
// --run             - execute the produced assembly when compilation succeeds (console apps only)

if (args.Length > 0 && string.Equals(args[0], "init", StringComparison.OrdinalIgnoreCase))
{
    Environment.ExitCode = RunInitCommand(args);
    return;
}

var sourceFiles = new List<string>();
var additionalRefs = new List<string>();
string? targetFrameworkTfm = null;
string? outputPath = null;
var outputKind = OutputKind.ConsoleApplication;
string? ravenCorePath = null;
var ravenCoreExplicitlyProvided = false;
var embedCoreTypes = false;
var skipDefaultRavenCoreLookup = false;
var allowUnsafe = false;

var printSyntaxTree = false;
var printSyntaxTreeInternal = false;
var syntaxTreeFormat = SyntaxTreeFormat.Flat;
var printSyntax = false;
var printRawSyntax = false;
var prettyIncludeDiagnostics = true;
var printBinders = false;
var printBoundTree = false;
var printBoundTreeErrors = true;
var symbolDumpMode = SymbolDumpMode.None;
var printParseSequence = false;
var parseSequenceThrottleMilliseconds = 0;
var showHelp = false;
var noEmit = false;
var fix = false;
var format = false;
var hasInvalidOption = false;
var highlightDiagnostics = false;
var showSuggestions = false;
var quote = false;
var quoteWithNamedArgs = false;
var runIlVerify = false;
string? ilVerifyPath = null;
var enableAsyncInvestigation = false;
string asyncInvestigationLabel = "Step14";
var asyncInvestigationScope = AsyncInvestigationPointerLabelScope.FieldOnly;
var enableOverloadLog = false;
string? overloadLogPath = null;
OverloadResolutionLog? overloadResolutionLog = null;
var run = false;
var emitDocs = false;
string? documentationOutputPath = null;
var documentationFormat = DocumentationFormat.Markdown;
var documentationTool = DocumentationTool.RavenDoc;
var documentationFormatExplicitlySet = false;

for (int i = 0; i < args.Length; i++)
{
    switch (args[i])
    {
        case "-o":
        case "--output":
            if (i + 1 < args.Length)
                outputPath = args[++i];
            break;
        case "-s":
        case "--syntax-tree":
            printSyntaxTree = true;
            if (!TryParseSyntaxTreeFormat(args, ref i, out syntaxTreeFormat))
                hasInvalidOption = true;
            break;
        case "-si":
        case "--syntax-tree--internal":
            printSyntaxTreeInternal = true;
            hasInvalidOption = false;
            break;
        case "--display-tree":
            printSyntaxTree = true;
            syntaxTreeFormat = SyntaxTreeFormat.Group;
            break;
        case "-se":
        case "--display-expanded-tree":
            printSyntaxTree = true;
            syntaxTreeFormat = SyntaxTreeFormat.Flat;
            break;
        case "-d":
        case "--dump":
            if (!TryParseSyntaxDumpFormat(args, ref i, out printRawSyntax, out printSyntax,
                    out prettyIncludeDiagnostics))
            {
                hasInvalidOption = true;
            }
            break;
        case "--print-syntax":
            printRawSyntax = true;
            printSyntax = false;
            break;
        case "-dp":
        case "--pretty-print":
            printRawSyntax = false;
            printSyntax = true;
            prettyIncludeDiagnostics = true;
            break;
        case "-b":
        case "--display-binders":
            printBinders = true;
            break;
        case "-bt":
        case "--bound-tree":
        case "--display-bound-tree":
            printBoundTree = true;
            break;
        case "-bte":
            printBoundTree = true;
            printBoundTreeErrors = true;
            break;
        case "-ps":
        case "--parse-sequence":
            printParseSequence = true;
            break;
        case "--ps-delay":
        case "--parse-sequence-delay":
            if (!TryParseNonNegativeInt(args, ref i, out parseSequenceThrottleMilliseconds))
                hasInvalidOption = true;
            break;
        case "--symbols":
        case "--dump-symbols":
            if (!TryParseSymbolDumpMode(args, ref i, out var mode))
                hasInvalidOption = true;
            else
                symbolDumpMode = mode;
            break;
        case "--no-emit":
            noEmit = true;
            break;
        case "--fix":
            fix = true;
            break;
        case "--format":
            format = true;
            break;
        case "--highlight":
            highlightDiagnostics = true;
            break;
        case "--suggestions":
            showSuggestions = true;
            break;
        case "--quote":
        case "-q":
            quote = true;
            if (i + 1 < args.Length && !args[i + 1].StartsWith('-'))
                quoteWithNamedArgs = args[++i] == "1";
            break;
        case "--doc":
        case "--emit-docs":
            emitDocs = true;
            if (i + 1 < args.Length && !args[i + 1].StartsWith('-'))
                documentationOutputPath = args[++i];
            break;
        case "--doc-output":
            emitDocs = true;
            if (i + 1 < args.Length)
                documentationOutputPath = args[++i];
            else
                hasInvalidOption = true;
            break;
        case "--doc-format":
            if (!TryParseDocumentationFormat(args, ref i, out var parsedFormat))
                hasInvalidOption = true;
            else
                documentationFormat = parsedFormat;
            documentationFormatExplicitlySet = true;
            break;
        case "--doc-tool":
            if (!TryParseDocumentationTool(args, ref i, out var parsedTool))
                hasInvalidOption = true;
            else
                documentationTool = parsedTool;
            break;
        case "--ilverify":
            runIlVerify = true;
            break;
        case "--ilverify-path":
            if (i + 1 < args.Length)
                ilVerifyPath = args[++i];
            else
                hasInvalidOption = true;
            break;
        case "--async-investigation":
            enableAsyncInvestigation = true;
            if (i + 1 < args.Length && !args[i + 1].StartsWith('-'))
                asyncInvestigationLabel = args[++i];
            break;
        case "--async-investigation-scope":
            if (i + 1 < args.Length)
            {
                var scopeValue = args[++i];
                if (string.Equals(scopeValue, "method", StringComparison.OrdinalIgnoreCase))
                {
                    asyncInvestigationScope = AsyncInvestigationPointerLabelScope.IncludeAsyncMethodName;
                }
                else if (!string.Equals(scopeValue, "field", StringComparison.OrdinalIgnoreCase))
                {
                    hasInvalidOption = true;
                }
            }
            else
            {
                hasInvalidOption = true;
            }
            break;
        case "--overload-log":
            enableOverloadLog = true;
            if (i + 1 < args.Length && !args[i + 1].StartsWith('-'))
                overloadLogPath = args[++i];
            break;
        case "--ref":
        case "--refs":
            if (i + 1 < args.Length)
                additionalRefs.Add(args[++i]);
            break;
        case "--raven-core":
            if (i + 1 < args.Length)
            {
                ravenCorePath = args[++i];
                embedCoreTypes = false;
                ravenCoreExplicitlyProvided = true;
            }
            else
            {
                hasInvalidOption = true;
            }
            break;
        case "--emit-core-types-only":
            embedCoreTypes = true;
            skipDefaultRavenCoreLookup = true;
            ravenCorePath = null;
            break;
        case "--run":
            run = true;
            break;
        case "--output-type":
            if (!TryParseOutputKind(args, ref i, out var kind))
                hasInvalidOption = true;
            else
                outputKind = kind;
            break;
        case "--unsafe":
            allowUnsafe = true;
            break;
        case "--framework":
            if (i + 1 < args.Length)
                targetFrameworkTfm = args[++i];
            break;
        case "-h":
        case "--help":
            showHelp = true;
            break;
        default:
            if (!args[i].StartsWith('-'))
                sourceFiles.Add(args[i]);
            break;
    }
}

if (showHelp || hasInvalidOption)
{
    if (hasInvalidOption)
        Environment.ExitCode = 1;
    else
        Environment.ExitCode = 0;

    PrintHelp();
    return;
}

if (printParseSequence)
{
    SyntaxParserFlags.PrintParseSequence = true;
    SyntaxParserFlags.ParseSequenceThrottleMilliseconds = parseSequenceThrottleMilliseconds;
}

if (sourceFiles.Count == 0)
    sourceFiles.Add($"../../../../../samples/result-and-options/test4{RavenFileExtensions.Raven}");

if (emitDocs && documentationTool == DocumentationTool.RavenDoc && documentationFormatExplicitlySet &&
    documentationFormat == DocumentationFormat.Xml)
{
    AnsiConsole.MarkupLine("[yellow]RavenDoc only supports Markdown documentation. Ignoring --doc-format xml.[/]");
    documentationFormat = DocumentationFormat.Markdown;
}

var inputMayBeProjectFile = sourceFiles.Count == 1 &&
                            string.Equals(Path.GetExtension(sourceFiles[0]), ".ravenproj", StringComparison.OrdinalIgnoreCase);

if (run && outputKind != OutputKind.ConsoleApplication && !inputMayBeProjectFile)
{
    AnsiConsole.MarkupLine("[red]--run is only supported for console applications. Class libraries cannot be executed.[/]");
    Environment.ExitCode = 1;
    return;
}

static bool IsValidAssemblyReference(string path)
{
    try
    {
        var fileInfo = new FileInfo(path);
        if (!fileInfo.Exists || fileInfo.Length == 0)
            return false;

        using var stream = File.OpenRead(path);
        using var peReader = new PEReader(stream);
        return peReader.HasMetadata;
    }
    catch
    {
        return false;
    }
}

if (!ravenCoreExplicitlyProvided && !skipDefaultRavenCoreLookup)
{
    var ravenCoreCandidates = new[]
    {
        Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"),
        Path.GetFullPath("../../../../../src/Raven.Core/bin/Debug/net9.0/Raven.Core.dll"),
        Path.GetFullPath("../../../../../src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll"),
    };

    foreach (var candidate in ravenCoreCandidates)
    {
        if (File.Exists(candidate) && IsValidAssemblyReference(candidate))
        {
            ravenCorePath = candidate;
            break;
        }
    }

    if (string.IsNullOrWhiteSpace(ravenCorePath))
        embedCoreTypes = true;
}
else if (string.IsNullOrWhiteSpace(ravenCorePath))
{
    embedCoreTypes = true;
}

for (int i = 0; i < sourceFiles.Count; i++)
{
    sourceFiles[i] = Path.GetFullPath(sourceFiles[i]);
    if (!File.Exists(sourceFiles[i]))
    {
        AnsiConsole.MarkupLine($"[red]Input file '{sourceFiles[i]}' doesn't exist.[/]");
        Environment.ExitCode = 1;
        return;
    }
}

var projectFileInput = sourceFiles.Count == 1 &&
                       string.Equals(Path.GetExtension(sourceFiles[0]), ".ravenproj", StringComparison.OrdinalIgnoreCase)
    ? sourceFiles[0]
    : null;

if (!string.IsNullOrWhiteSpace(ravenCorePath))
{
    ravenCorePath = Path.GetFullPath(ravenCorePath);
    if (!File.Exists(ravenCorePath))
    {
        if (ravenCoreExplicitlyProvided)
        {
            AnsiConsole.MarkupLine($"[red]Raven core assembly '{ravenCorePath}' doesn't exist.[/]");
            Environment.ExitCode = 1;
            return;
        }

        ravenCorePath = null;
    }
    else if (!IsValidAssemblyReference(ravenCorePath))
    {
        if (ravenCoreExplicitlyProvided)
        {
            AnsiConsole.MarkupLine($"[red]Raven core assembly '{ravenCorePath}' is not a valid managed assembly.[/]");
            Environment.ExitCode = 1;
            return;
        }

        ravenCorePath = null;
    }
}

var explicitOutputPath = !string.IsNullOrEmpty(outputPath);
var defaultAssemblyBaseName = projectFileInput is not null
    ? Path.GetFileNameWithoutExtension(projectFileInput)
    : Path.GetFileNameWithoutExtension(sourceFiles[0]);

string outputFilePath;
string assemblyName;
string outputDirectory;
if (projectFileInput is not null)
{
    if (explicitOutputPath && Path.HasExtension(outputPath))
    {
        AnsiConsole.MarkupLine("[red]For project-file inputs, -o/--output must be a directory path, not a file path.[/]");
        Environment.ExitCode = 1;
        return;
    }

    var projectDirectory = Path.GetDirectoryName(projectFileInput)!;
    outputDirectory = explicitOutputPath
        ? Path.GetFullPath(outputPath!)
        : Path.Combine(projectDirectory, "bin");
    assemblyName = defaultAssemblyBaseName;
    outputFilePath = Path.Combine(outputDirectory, $"{assemblyName}.dll");
}
else
{
    outputPath = !string.IsNullOrEmpty(outputPath) ? outputPath : defaultAssemblyBaseName;
    if (!Path.HasExtension(outputPath))
        outputPath = $"{outputPath}.dll";
    outputFilePath = Path.GetFullPath(outputPath);
    assemblyName = Path.GetFileNameWithoutExtension(outputFilePath);
    outputDirectory = Path.GetDirectoryName(outputFilePath)!;
    if (string.IsNullOrEmpty(outputDirectory))
        outputDirectory = Directory.GetCurrentDirectory();
}
if (!string.IsNullOrEmpty(ravenCorePath))
{
    Directory.CreateDirectory(outputDirectory!);
    var ravenCoreDestination = Path.Combine(outputDirectory!, Path.GetFileName(ravenCorePath));

    // When targeting a project output directory, Raven.Core.dll may already be loaded by a running app.
    // In that case, overwriting can fail on some platforms. Prefer reusing the existing file if it looks identical.
    if (!string.Equals(ravenCoreDestination, ravenCorePath, StringComparison.OrdinalIgnoreCase))
    {
        try
        {
            if (File.Exists(ravenCoreDestination))
            {
                var srcInfo = new FileInfo(ravenCorePath);
                var dstInfo = new FileInfo(ravenCoreDestination);

                // Fast equality check: same size and same last write time (UTC). If so, keep the existing copy.
                if (srcInfo.Exists && dstInfo.Exists &&
                    srcInfo.Length == dstInfo.Length &&
                    srcInfo.LastWriteTimeUtc == dstInfo.LastWriteTimeUtc)
                {
                    ravenCorePath = ravenCoreDestination;
                }
                else
                {
                    File.Copy(ravenCorePath, ravenCoreDestination, overwrite: true);
                    // Preserve timestamps so subsequent runs can use the fast equality check.
                    File.SetLastWriteTimeUtc(ravenCoreDestination, srcInfo.LastWriteTimeUtc);
                    ravenCorePath = ravenCoreDestination;
                }
            }
            else
            {
                File.Copy(ravenCorePath, ravenCoreDestination, overwrite: true);
                var srcInfo = new FileInfo(ravenCorePath);
                if (srcInfo.Exists)
                    File.SetLastWriteTimeUtc(ravenCoreDestination, srcInfo.LastWriteTimeUtc);
                ravenCorePath = ravenCoreDestination;
            }
        }
        catch (IOException)
        {
            // If the destination is locked (e.g. a previously run app is still using Raven.Core.dll),
            // keep using the existing file if present.
            if (File.Exists(ravenCoreDestination))
            {
                AnsiConsole.MarkupLine(
                    $"[yellow]Warning: Could not overwrite '{Markup.Escape(ravenCoreDestination)}' because it is in use. Reusing the existing file.[/]");
                ravenCorePath = ravenCoreDestination;
            }
            else
            {
                throw;
            }
        }
    }
}

string? ResolveAndCopyLocalDependency(string fileName, params string[] candidates)
{
    foreach (var candidate in candidates)
    {
        if (string.IsNullOrWhiteSpace(candidate))
            continue;

        var full = Path.GetFullPath(candidate);
        if (!File.Exists(full) || !IsValidAssemblyReference(full))
            continue;

        Directory.CreateDirectory(outputDirectory!);
        var destination = Path.Combine(outputDirectory!, fileName);

        if (!string.Equals(full, destination, StringComparison.OrdinalIgnoreCase))
        {
            try
            {
                // Same "in use" issue can happen for local dependencies when the previous output is still running.
                if (File.Exists(destination))
                {
                    var srcInfo = new FileInfo(full);
                    var dstInfo = new FileInfo(destination);
                    if (srcInfo.Exists && dstInfo.Exists &&
                        srcInfo.Length == dstInfo.Length &&
                        srcInfo.LastWriteTimeUtc == dstInfo.LastWriteTimeUtc)
                    {
                        return destination;
                    }
                }

                File.Copy(full, destination, overwrite: true);
                var src = new FileInfo(full);
                if (src.Exists)
                    File.SetLastWriteTimeUtc(destination, src.LastWriteTimeUtc);
            }
            catch (IOException)
            {
                if (File.Exists(destination))
                {
                    AnsiConsole.MarkupLine(
                        $"[yellow]Warning: Could not overwrite '{Markup.Escape(destination)}' because it is in use. Reusing the existing file.[/]");
                    return destination;
                }

                throw;
            }
        }

        return destination;
    }

    return null;
}

var testDepPath = ResolveAndCopyLocalDependency(
    "TestDep.dll",
    Path.Combine(AppContext.BaseDirectory, "TestDep.dll"),
    Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "TestDep.dll"),
    Path.GetFullPath("../../../../../src/TestDep/bin/Debug/net9.0/TestDep.dll"),
    Path.GetFullPath("../../../../../src/TestDep/bin/Debug/net9.0/net9.0/TestDep.dll"));

var ravenCodeAnalysisPath = ResolveAndCopyLocalDependency(
    "Raven.CodeAnalysis.dll",
    Path.Combine(AppContext.BaseDirectory, "Raven.CodeAnalysis.dll"),
    Path.GetFullPath("../../../../../src/Raven.CodeAnalysis/bin/Debug/net9.0/Raven.CodeAnalysis.dll"),
    Path.GetFullPath("../../../../../src/Raven.CodeAnalysis/bin/Debug/net9.0/net9.0/Raven.CodeAnalysis.dll"));

var targetFramework = targetFrameworkTfm ?? TargetFrameworkUtil.GetLatestFramework();
var version = TargetFrameworkResolver.ResolveVersion(targetFramework);

var executionOptions = new CompilerExecutionOptions(
    outputKind,
    allowUnsafe,
    enableAsyncInvestigation,
    asyncInvestigationLabel,
    asyncInvestigationScope,
    embedCoreTypes,
    enableOverloadLog,
    overloadLogPath);
var optionsResult = CreateCompilationOptions(executionOptions);
var options = optionsResult.Options;
overloadResolutionLog = optionsResult.OverloadResolutionLog;
var workspace = RavenWorkspace.Create(targetFramework: targetFramework);
workspace.Services.SyntaxTreeProvider.ParseOptions = new ParseOptions
{
    DocumentationMode = true,
    DocumentationFormat = documentationFormat
};
ProjectId projectId;
Project project;

if (projectFileInput is not null)
{
    projectId = workspace.OpenProject(projectFileInput);
    project = workspace.CurrentSolution.GetProject(projectId)!;

    if (targetFrameworkTfm is not null)
    {
        var frameworkReferences = TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
        foreach (var reference in frameworkReferences)
            project = project.AddMetadataReference(reference);
    }
}
else
{
    projectId = workspace.AddProject(assemblyName, compilationOptions: options);
    project = workspace.CurrentSolution.GetProject(projectId)!;

    foreach (var filePath in sourceFiles)
    {
        using var file = File.OpenRead(filePath);
        var sourceText = SourceText.From(file);
        var parsedTree = SyntaxTree.ParseText(sourceText, path: filePath);
        var document = project.AddDocument(Path.GetFileName(filePath), sourceText, filePath);
        project = document.Project;
    }

    var frameworkReferences = TargetFrameworkResolver.GetReferenceAssemblies(version)
        .Select(MetadataReference.CreateFromFile)
        .ToArray();

    foreach (var reference in frameworkReferences)
    {
        project = project.AddMetadataReference(reference);
    }
}

if (!string.IsNullOrWhiteSpace(ravenCorePath))
{
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(ravenCorePath));
}

if (!string.IsNullOrWhiteSpace(testDepPath))
{
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(testDepPath));
}
else
{
    AnsiConsole.MarkupLine("[yellow]Warning: Could not locate TestDep.dll; compilation may fail unless you pass --refs.[/]");
}

if (!string.IsNullOrWhiteSpace(ravenCodeAnalysisPath))
{
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(ravenCodeAnalysisPath));
}
else
{
    AnsiConsole.MarkupLine("[yellow]Warning: Could not locate Raven.CodeAnalysis.dll; compilation may fail unless you pass --refs.[/]");
}

foreach (var r in additionalRefs)
{
    var full = Path.GetFullPath(r);
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(full));
}

if (projectFileInput is not null)
{
    options = project.CompilationOptions ?? options;
    assemblyName = project.AssemblyName ?? project.Name;
    outputFilePath = Path.Combine(outputDirectory, $"{assemblyName}.dll");
}

project = project.WithCompilationOptions(options);
project = AddDefaultAnalyzers(project);

workspace.TryApplyChanges(project.Solution);
project = workspace.CurrentSolution.GetProject(projectId)!;

if (!noEmit)
{
    CopyNuGetReferencesToOutput(project, targetFramework, outputDirectory!);
}

if (run && options.OutputKind != OutputKind.ConsoleApplication)
{
    AnsiConsole.MarkupLine("[red]--run is only supported for console applications. Class libraries cannot be executed.[/]");
    Environment.ExitCode = 1;
    return;
}

project = ApplyCodeFixesIfRequested(workspace, projectId, project, fix);
project = ApplyFormattingIfRequested(workspace, projectId, project, format);

// Print syntax tree BEFORE binding/diagnostics retrieval.
// Binding may be triggered by workspace diagnostics/semantic model creation, so keep syntax-only printing above that.
var allowConsoleOutputPreBinding = project.Documents.Count() == 1;
if (allowConsoleOutputPreBinding && (printSyntaxTree || printSyntaxTreeInternal))
{
    var document = project.Documents.Single();
    var syntaxTree = document.GetSyntaxTreeAsync().Result!;
    var root = syntaxTree.GetRoot();

    if (printSyntaxTree)
    {
        root.PrintSyntaxTree(new PrinterOptions
        {
            IncludeNames = true,
            IncludeTokens = true,
            IncludeTrivia = true,
            IncludeSpans = true,
            IncludeLocations = true,
            Colorize = true,
            ExpandListsAsProperties = syntaxTreeFormat == SyntaxTreeFormat.Flat,
            // IMPORTANT: keep this syntax-only. Do not include diagnostics here, since fetching diagnostics can bind.
            IncludeDiagnostics = false,
            IncludeAnnotations = true,
            DiagnosticsAsChildren = false,
            AnnotationsAsChildren = true
        });
    }
    else
    {
        // Internal (green) syntax tree printer.
        var green = (GreenNode?)typeof(SyntaxNode)
            .GetField("Green", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)!
            .GetValue(root);

        var greenPrinterOptions = new Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinterOptions()
        {
            IncludeTrivia = true,
            IncludeTriviaText = true,
            FlattenSyntaxLists = false,
            IncludeSlotIndices = true,
            IncludeWidths = true,
            HideZeroWidthSlots = false,
            IncludeDiagnostics = false,
            IncludeAnnotations = true,
            DiagnosticsAsChildren = false,
            AnnotationsAsChildren = true
        };

        Console.WriteLine(Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinter.PrintToString(green!, greenPrinterOptions));
    }

    // Avoid printing the same tree again later.
    printSyntaxTree = false;
    printSyntaxTreeInternal = false;
}

var compilation = workspace.GetCompilation(projectId);

var diagnostics = workspace.GetDiagnostics(projectId);

EmitResult? result = null;
if (!noEmit)
{
    using (var stream = File.Open(outputFilePath, FileMode.Create, FileAccess.Write, FileShare.None))
    {
        result = compilation.Emit(stream);
    }

    diagnostics = diagnostics.Concat(result!.Diagnostics).Distinct().ToImmutableArray();
}

if (emitDocs)
{
    if (documentationTool == DocumentationTool.RavenDoc)
    {
        documentationOutputPath ??= Path.Combine(outputDirectory!, $"{assemblyName}.docs");
        DocumentationGenerator.ProcessCompilation(compilation, documentationOutputPath);
        AnsiConsole.MarkupLine($"[green]Documentation written to '{documentationOutputPath}'.[/]");
    }
    else
    {
        var formatExtension = documentationFormat == DocumentationFormat.Markdown ? ".md" : ".xml";
        documentationOutputPath ??= Path.ChangeExtension(outputFilePath, formatExtension);

        DocumentationEmitter.WriteDocumentation(project.Documents, documentationFormat, documentationOutputPath);
        AnsiConsole.MarkupLine($"[green]Documentation written to '{documentationOutputPath}'.[/]");
    }
}

stopwatch.Stop();

var allowConsoleOutput = project.Documents.Count() == 1;
var debugDir = FindDebugDirectory();
var ilVerifyFailed = false;

if (debugDir is not null)
{
    Directory.CreateDirectory(debugDir);

    foreach (var document in project.Documents)
    {
        var syntaxTree = document.GetSyntaxTreeAsync().Result!;
        var root = syntaxTree.GetRoot();
        var name = Path.GetFileNameWithoutExtension(document.FilePath) ?? document.Name;

        File.WriteAllText(Path.Combine(debugDir, $"{name}.raw.rav"), root.ToFullString());

        var treeText = root.GetSyntaxTreeRepresentation(new PrinterOptions
        {
            IncludeNames = true,
            IncludeTokens = true,
            IncludeTrivia = true,
            IncludeSpans = true,
            IncludeLocations = true,
            Colorize = false,
            ExpandListsAsProperties = true,
            IncludeDiagnostics = true,
            IncludeAnnotations = true,
            DiagnosticsAsChildren = true,
            AnnotationsAsChildren = true,
        }).StripAnsiCodes();

        File.WriteAllText(Path.Combine(debugDir, $"{name}.syntax-tree.txt"), treeText);

        ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
        var syntaxDiagnostics = diagnostics.Where(d => d.Location.SourceTree == syntaxTree);
        var syntax = root.WriteNodeToText(compilation, includeDiagnostics: true, diagnostics: syntaxDiagnostics)
            .StripAnsiCodes();
        File.WriteAllText(Path.Combine(debugDir, $"{name}.syntax.txt"), syntax);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        using (var sw = new StringWriter())
        {
            var original = Console.Out;
            Console.SetOut(sw);
            semanticModel.PrintBinderTree(colorize: false);
            Console.SetOut(original);
            File.WriteAllText(Path.Combine(debugDir, $"{name}.binders.txt"), sw.ToString());
        }

        using (var sw = new StringWriter())
        {
            var original = Console.Out;
            Console.SetOut(sw);
            semanticModel.PrintBoundTree(includeChildPropertyNames: true, groupChildCollections: true, colorize: false);
            Console.SetOut(original);
            File.WriteAllText(Path.Combine(debugDir, $"{name}.bound-tree.txt"), sw.ToString());
        }
    }

    AnsiConsole.MarkupLine($"[yellow]Debug output written to '{debugDir}'.[/]");
}

if (allowConsoleOutput)
{
    var document = project.Documents.Single();
    var syntaxTree = document.GetSyntaxTreeAsync().Result!;
    var root = syntaxTree.GetRoot();

    if (printRawSyntax)
    {
        Console.WriteLine(root.ToFullString());
        Console.WriteLine();
    }

    if (printSyntaxTree)
    {
        var includeLocations = true;
        root.PrintSyntaxTree(new PrinterOptions
        {
            IncludeNames = true,
            IncludeTokens = true,
            IncludeTrivia = true,
            IncludeSpans = true,
            IncludeLocations = includeLocations,
            Colorize = true,
            ExpandListsAsProperties = syntaxTreeFormat == SyntaxTreeFormat.Flat,
            IncludeDiagnostics = true,
            IncludeAnnotations = true,
            DiagnosticsAsChildren = true,
            AnnotationsAsChildren = true
        });
    }
    else if (printSyntaxTreeInternal)
    {
        var green = (GreenNode?)typeof(SyntaxNode)
            .GetField("Green", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)!
            .GetValue(root);

        var greenPrinterOptions = new Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinterOptions()
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
        };

        Console.WriteLine(Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinter.PrintToString(green!, greenPrinterOptions));
    }

    if (printSyntax)
    {
        ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
        var prettyDiagnostics = prettyIncludeDiagnostics
            ? diagnostics.Where(d => d.Location.SourceTree == syntaxTree)
            : null;
        var highlighted = root.WriteNodeToText(compilation, includeDiagnostics: prettyIncludeDiagnostics,
            diagnostics: prettyDiagnostics);
        if (highlighted.Length > 0)
        {
            Console.WriteLine(highlighted);
            Console.WriteLine();
        }
        else
        {
            Console.WriteLine();
        }
    }

    if (printBinders || printBoundTree)
    {
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        if (printBinders)
        {
            semanticModel.PrintBinderTree();
            Console.WriteLine();
        }

        if (printBoundTree)
        {
            if (!printBinders)
            {
                semanticModel.PrintBinderTree();
                Console.WriteLine();
            }

            semanticModel.PrintBoundTree(includeChildPropertyNames: true, groupChildCollections: true, displayCollectionIndices: false, onlyBlockRoots: !printBoundTreeErrors, includeErrorNodes: printBoundTreeErrors);
            Console.WriteLine();
        }
    }
}
else if (printRawSyntax || printSyntaxTree || printSyntax || printBinders || printBoundTree)
{
    if (debugDir is null)
        AnsiConsole.MarkupLine("[yellow]Create a '.debug' directory to capture debug output.[/]");
}

if (symbolDumpMode != SymbolDumpMode.None)
{
    var filter = static (ISymbol symbol) =>
        symbol.Kind is SymbolKind.Assembly or SymbolKind.Module or SymbolKind.Namespace ||
        !symbol.DeclaringSyntaxReferences.IsDefaultOrEmpty;

    switch (symbolDumpMode)
    {
        case SymbolDumpMode.Hierarchy:
            {
                var symbolDump = compilation.Assembly.ToSymbolHierarchyString(filter);
                Console.WriteLine(symbolDump);
                Console.WriteLine();
                break;
            }

        case SymbolDumpMode.List:
            {
                var symbolDump = compilation.Assembly.ToSymbolListString(filter);
                Console.WriteLine(symbolDump);
                Console.WriteLine();
                break;
            }
    }
}

if (quote)
{
    var root = compilation.SyntaxTrees.First().GetRoot();
    var quoted = RavenQuoter.Quote(root, new RavenQuoterOptions
    {
        IncludeTrivia = true,
        GenerateUsingDirectives = true,
        UseStaticSyntaxFactoryImport = true,
        UseNamedArguments = quoteWithNamedArgs,
        IgnoreNullValue = true,
        InlineSingleArg = false,
        UseFactoryPropsForSimpleTokens = true
    });

    Console.WriteLine(quoted);
    Console.WriteLine();
}

if (runIlVerify)
{
    if (noEmit)
    {
        AnsiConsole.MarkupLine("[red]--ilverify requires the assembly to be emitted. Remove --no-emit to run the verifier.[/]");
        ilVerifyFailed = true;
    }
    else if (result is null)
    {
        ilVerifyFailed = true;
    }
    else if (result.Success)
    {
        ilVerifyFailed = !IlVerifyRunner.Verify(ilVerifyPath, outputFilePath, compilation);
    }
}

if (diagnostics.Length > 0)
{
    PrintDiagnostics(diagnostics, compilation, highlightDiagnostics, showSuggestions);
    Console.WriteLine();
}

var errors = diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error && d.Descriptor.Id != "RAV1011");
var emitFailed = result is { Success: false };

if (errors.Any() || emitFailed || ilVerifyFailed)
{
    Environment.ExitCode = 1;
    if (emitFailed && result is not null)
    {
        Failed(result);
    }
    else if (errors.Any() && result is not null)
        Failed(result);
    else if (errors.Any())
        Failed(errors.Count());
    else
        Failed(1);
}
else
{
    var warningsCount = diagnostics.Count(x => x.Severity == DiagnosticSeverity.Warning);
    if (warningsCount > 0)
    {
        Environment.ExitCode = 0;
        SucceededWithWarnings(warningsCount, stopwatch.Elapsed);
    }
    else
    {
        Environment.ExitCode = 0;
        Succeeded(stopwatch.Elapsed);
    }

    if (result is not null)
        CreateAppHost(compilation, outputFilePath, targetFramework);

    if (run)
    {
        if (noEmit || result is null)
        {
            AnsiConsole.MarkupLine("[yellow]--run was specified but no assembly was emitted to execute.[/]");
        }
        else if (result.Success)
        {
            RunAssembly(outputFilePath);
        }
    }
}

overloadResolutionLog?.Dispose();

static Project AddDefaultAnalyzers(Project project)
{
    return project
        .AddAnalyzerReference(new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new PreferTargetTypedUnionCaseAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new EventDelegateMustBeNullableAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new NonNullDeclarationsAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new VarCanBeValAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new MatchExhaustivenessAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new PreferValInsteadOfLetAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new AutoPropertyInitializationAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new PreferNewLineBetweenDeclarationsAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new ThrowStatementUseResultAnalyzer()))
        .AddAnalyzerReference(new AnalyzerReference(new PreferDuLinqExtensionsAnalyzer()));
}

static (CompilationOptions Options, OverloadResolutionLog? OverloadResolutionLog) CreateCompilationOptions(
    CompilerExecutionOptions executionOptions)
{
    var options = new CompilationOptions(executionOptions.OutputKind)
        .WithAllowUnsafe(executionOptions.AllowUnsafe)
        .WithEmbedCoreTypes(executionOptions.EmbedCoreTypes);

    if (executionOptions.EnableAsyncInvestigation)
    {
        options = options.WithAsyncInvestigation(
            AsyncInvestigationOptions.Enable(
                executionOptions.AsyncInvestigationLabel,
                executionOptions.AsyncInvestigationScope));
    }

    OverloadResolutionLog? overloadResolutionLog = null;
    if (executionOptions.EnableOverloadLog)
    {
        var ownsWriter = !string.IsNullOrWhiteSpace(executionOptions.OverloadLogPath);
        var writer = ownsWriter
            ? new StreamWriter(File.Open(executionOptions.OverloadLogPath!, FileMode.Create, FileAccess.Write, FileShare.Read))
            : Console.Out;

        overloadResolutionLog = new OverloadResolutionLog(writer, ownsWriter);
        options = options.WithOverloadResolutionLogger(overloadResolutionLog);
    }

    return (options, overloadResolutionLog);
}

static Project ApplyCodeFixesIfRequested(RavenWorkspace workspace, ProjectId projectId, Project project, bool fix)
{
    if (!fix)
        return project;

    var applyResult = workspace.ApplyCodeFixes(
        projectId,
        BuiltInCodeFixProviders.CreateDefault());

    if (applyResult.AppliedFixCount == 0)
        return project;

    workspace.TryApplyChanges(applyResult.Solution);
    project = workspace.CurrentSolution.GetProject(projectId)!;

    foreach (var appliedFix in applyResult.AppliedFixes)
    {
        var document = workspace.CurrentSolution.GetDocument(appliedFix.DocumentId);
        var path = document?.FilePath ?? document?.Name ?? "<unknown>";
        var lineSpan = appliedFix.Diagnostic.Location.GetLineSpan();
        var line = lineSpan.StartLinePosition.Line + 1;
        var column = lineSpan.StartLinePosition.Character + 1;
        var diagnosticId = appliedFix.Diagnostic.Id;
        var displayPath = path;
        if (!string.IsNullOrWhiteSpace(path) && Path.IsPathRooted(path))
            displayPath = Path.GetRelativePath(Environment.CurrentDirectory, path);

        var escapedPath = Markup.Escape(displayPath);
        AnsiConsole.MarkupLine(
            $"[grey]{escapedPath}({line},{column}):[/] [blue]{diagnosticId}[/] {appliedFix.Action.Title}");
    }

    WriteProjectDocumentsToDisk(project);
    AnsiConsole.MarkupLine($"[green]Applied {applyResult.AppliedFixCount} code fix(es).[/]");
    return project;
}

static Project ApplyFormattingIfRequested(RavenWorkspace workspace, ProjectId projectId, Project project, bool format)
{
    if (!format)
        return project;

    var formattedDocumentCount = 0;
    var formattedSolution = project.Solution;

    foreach (var document in project.Documents)
    {
        var currentDocument = formattedSolution.GetDocument(document.Id);
        if (currentDocument is null)
            continue;

        var syntaxTree = currentDocument.GetSyntaxTreeAsync().GetAwaiter().GetResult();
        if (syntaxTree is null)
            continue;

        var root = syntaxTree.GetRoot();
        var normalizedRoot = root.NormalizeWhitespace();
        if (string.Equals(root.ToFullString(), normalizedRoot.ToFullString(), StringComparison.Ordinal))
            continue;

        formattedSolution = currentDocument.WithSyntaxRoot(normalizedRoot).Project.Solution;
        formattedDocumentCount++;
    }

    if (formattedDocumentCount == 0)
    {
        AnsiConsole.MarkupLine("[green]All files are already formatted.[/]");
        return project;
    }

    workspace.TryApplyChanges(formattedSolution);
    project = workspace.CurrentSolution.GetProject(projectId)!;

    var updatedDocumentCount = WriteProjectDocumentsToDisk(project);
    AnsiConsole.MarkupLine($"[green]Formatted {updatedDocumentCount} file(s).[/]");
    return project;
}

static int WriteProjectDocumentsToDisk(Project project)
{
    var updatedDocumentCount = 0;

    foreach (var document in project.Documents)
    {
        if (string.IsNullOrWhiteSpace(document.FilePath))
            continue;

        var text = document.GetTextAsync().GetAwaiter().GetResult();
        var newText = text.ToString();
        var existingText = File.Exists(document.FilePath) ? File.ReadAllText(document.FilePath) : null;
        if (string.Equals(existingText, newText, StringComparison.Ordinal))
            continue;

        var encoding = text.Encoding ?? Encoding.UTF8;
        if (encoding is UTF8Encoding)
            encoding = new UTF8Encoding(encoderShouldEmitUTF8Identifier: false);

        File.WriteAllText(document.FilePath, newText, encoding);
        updatedDocumentCount++;
    }

    return updatedDocumentCount;
}

static string? FindDebugDirectory()
{
    var dir = Environment.CurrentDirectory;
    while (dir is not null)
    {
        var debug = Path.Combine(dir, ".debug");
        if (Directory.Exists(debug))
            return debug;

        dir = Path.GetDirectoryName(dir);
    }

    return null;
}

static void CopyNuGetReferencesToOutput(Project project, string targetFramework, string outputDirectory)
{
    var globalPackages = GetNuGetGlobalPackagesFolder();
    if (string.IsNullOrWhiteSpace(globalPackages) || !Directory.Exists(globalPackages))
        return;

    var copiedFileNames = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

    foreach (var reference in project.MetadataReferences.OfType<PortableExecutableReference>())
    {
        var referencePath = reference.FilePath;
        if (string.IsNullOrWhiteSpace(referencePath))
            continue;

        var fullReferencePath = Path.GetFullPath(referencePath);
        var isNuGetPackageReference = IsUnderDirectory(fullReferencePath, globalPackages);
        var isSharedFrameworkReference = IsUnderAnyDotNetSharedRoot(fullReferencePath);
        if (!isNuGetPackageReference && !isSharedFrameworkReference)
            continue;

        var runtimePath = isNuGetPackageReference
            ? ResolveNuGetRuntimeAssemblyPath(fullReferencePath, targetFramework)
            : fullReferencePath;
        if (!File.Exists(runtimePath))
            continue;

        Directory.CreateDirectory(outputDirectory);
        var fileName = Path.GetFileName(runtimePath);
        var destination = Path.Combine(outputDirectory, fileName);

        if (copiedFileNames.TryGetValue(fileName, out var existingSource) &&
            !string.Equals(existingSource, runtimePath, StringComparison.OrdinalIgnoreCase))
        {
            AnsiConsole.MarkupLine(
                $"[yellow]Warning: Skipping NuGet reference '{Markup.Escape(runtimePath)}' because '{Markup.Escape(fileName)}' was already copied from '{Markup.Escape(existingSource)}'.[/]");
            continue;
        }

        copiedFileNames[fileName] = runtimePath;

        if (!string.Equals(runtimePath, destination, StringComparison.OrdinalIgnoreCase))
            File.Copy(runtimePath, destination, overwrite: true);
    }
}

static string ResolveNuGetRuntimeAssemblyPath(string referencePath, string targetFramework)
{
    var normalized = referencePath.Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar);
    var refSegment = $"{Path.DirectorySeparatorChar}ref{Path.DirectorySeparatorChar}";
    var refIndex = normalized.IndexOf(refSegment, StringComparison.OrdinalIgnoreCase);
    if (refIndex < 0)
        return referencePath;

    var libCandidate = normalized[..refIndex] +
                       $"{Path.DirectorySeparatorChar}lib{Path.DirectorySeparatorChar}" +
                       normalized[(refIndex + refSegment.Length)..];
    if (File.Exists(libCandidate))
        return libCandidate;

    var packageRoot = FindNuGetPackageRoot(normalized, refIndex);
    if (packageRoot is null)
        return referencePath;

    var fileName = Path.GetFileName(referencePath);
    var targetCandidate = Path.Combine(packageRoot, "lib", targetFramework, fileName);
    if (File.Exists(targetCandidate))
        return targetCandidate;

    var alternatives = Directory.Exists(Path.Combine(packageRoot, "lib"))
        ? Directory.EnumerateFiles(Path.Combine(packageRoot, "lib"), fileName, SearchOption.AllDirectories)
            .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
        : Enumerable.Empty<string>();

    return alternatives.FirstOrDefault() ?? referencePath;
}

static string? FindNuGetPackageRoot(string normalizedReferencePath, int refSegmentStartIndex)
{
    // Path shape: .../<id>/<version>/ref/<tfm>/<assembly>.dll
    var beforeRef = normalizedReferencePath[..refSegmentStartIndex];
    var versionDir = Path.GetDirectoryName(beforeRef);
    if (string.IsNullOrWhiteSpace(versionDir))
        return null;
    var packageDir = Path.GetDirectoryName(versionDir);
    if (string.IsNullOrWhiteSpace(packageDir))
        return null;
    return Path.Combine(packageDir, Path.GetFileName(versionDir));
}

static string GetNuGetGlobalPackagesFolder()
{
    var env = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
    if (!string.IsNullOrWhiteSpace(env))
        return Path.GetFullPath(env);

    var userProfile = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
    if (string.IsNullOrWhiteSpace(userProfile))
        return string.Empty;

    return Path.Combine(userProfile, ".nuget", "packages");
}

static bool IsUnderDirectory(string path, string directory)
{
    var fullPath = Path.GetFullPath(path)
        .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
    var fullDirectory = Path.GetFullPath(directory)
        .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
    return fullPath.StartsWith(fullDirectory + Path.DirectorySeparatorChar, StringComparison.OrdinalIgnoreCase) ||
           string.Equals(fullPath, fullDirectory, StringComparison.OrdinalIgnoreCase);
}

static bool IsUnderAnyDotNetSharedRoot(string path)
{
    foreach (var root in GetDotNetRootsForDependencyCopy())
    {
        var sharedRoot = Path.Combine(root, "shared");
        if (Directory.Exists(sharedRoot) && IsUnderDirectory(path, sharedRoot))
            return true;
    }

    return false;
}

static IEnumerable<string> GetDotNetRootsForDependencyCopy()
{
    var envRoots = new[]
    {
        Environment.GetEnvironmentVariable("DOTNET_ROOT"),
        Environment.GetEnvironmentVariable("DOTNET_ROOT(x86)")
    }.Where(static value => !string.IsNullOrWhiteSpace(value));

    foreach (var root in envRoots)
    {
        if (Directory.Exists(root))
            yield return root!;
    }

    if (OperatingSystem.IsWindows())
    {
        var x64 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "dotnet");
        var x86 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "dotnet");
        if (Directory.Exists(x64))
            yield return x64;
        if (Directory.Exists(x86))
            yield return x86;
    }
    else if (OperatingSystem.IsMacOS())
    {
        const string appleDefault = "/usr/local/share/dotnet";
        const string brew = "/opt/homebrew/opt/dotnet/libexec";
        if (Directory.Exists(appleDefault))
            yield return appleDefault;
        if (Directory.Exists(brew))
            yield return brew;
    }
    else
    {
        var candidates = new[]
        {
            "/usr/share/dotnet",
            "/usr/lib/dotnet",
            "/snap/dotnet-sdk/current",
        };

        foreach (var candidate in candidates)
        {
            if (Directory.Exists(candidate))
                yield return candidate;
        }
    }
}

static void PrintHelp()
{
    Console.WriteLine("Usage: ravenc [options] <source-files|project-file.ravenproj>");
    Console.WriteLine("       ravenc init [--name <project-name>] [--framework <tfm>] [--force]");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("  --framework <tfm>  Target framework (e.g. net8.0)");
    Console.WriteLine("  --refs <path>      Additional metadata reference (repeatable)");
    Console.WriteLine("  --raven-core <path> Reference a prebuilt Raven.Core.dll instead of embedding compiler shims");
    Console.WriteLine("  --emit-core-types-only Embed Raven.Core shims even when Raven.Core.dll is available");
    Console.WriteLine("  --emit-docs       Emit documentation from comments");
    Console.WriteLine("  --output-type <console|classlib>");
    Console.WriteLine("                     Output kind for the produced assembly.");
    Console.WriteLine("  --unsafe         Enable unsafe mode (required for pointer declarations/usages)");
    Console.WriteLine("  -o <path>          Output path.");
    Console.WriteLine("                     For .rav inputs: output assembly path.");
    Console.WriteLine("                     For .ravenproj inputs: output directory path (default: <project-dir>/bin).");
    Console.WriteLine("  -s [flat|group]    Display the syntax tree (single file only)");
    Console.WriteLine("                     Use 'group' to display syntax lists grouped by property.");
    Console.WriteLine("  -ps                Print the parsing sequence");
    Console.WriteLine("  --ps-delay <ms>    Delay each parse-sequence log line by <ms> milliseconds");
    Console.WriteLine("  -d [plain|pretty[:no-diagnostics]] Dump syntax (single file only)");
    Console.WriteLine("                     'plain' writes the source text, 'pretty' writes highlighted syntax.");
    Console.WriteLine("                     Append ':no-diagnostics' to skip diagnostic underlines when using 'pretty'.");
    Console.WriteLine("  -r                 Print the source (single file only)");
    Console.WriteLine("  -b                 Print binder tree (single file only)");
    Console.WriteLine("  -bt                Print binder and bound tree (single file only)");
    Console.WriteLine("  -bte               Print binder and bound tree with error nodes (single file only)");
    Console.WriteLine("  --symbols [list|hierarchy]");
    Console.WriteLine("                     Inspect symbols produced from source.");
    Console.WriteLine("                     'list' dumps properties, 'hierarchy' prints the tree.");
    Console.WriteLine("  --overload-log [path]");
    Console.WriteLine("                     Log overload resolution details to the console or the provided file.");
    Console.WriteLine("  --highlight       Display diagnostics with highlighted source snippets");
    Console.WriteLine("  --suggestions    Display educational rewrite suggestions for diagnostics that provide them");
    Console.WriteLine("  -q                 Display AST as compilable C# code");
    Console.WriteLine("  --no-emit        Skip emitting the output assembly");
    Console.WriteLine("  --fix            Apply supported code fixes to source files before compiling");
    Console.WriteLine("  --format         Normalize whitespace and indentation in source files before compiling");
    Console.WriteLine("  --doc-tool [ravendoc|comments]");
    Console.WriteLine("                    Documentation generator to use (default: ravendoc).");
    Console.WriteLine("  --doc-format [md|xml]");
    Console.WriteLine("                    Documentation format for comment emission (default: md).");
    Console.WriteLine("  --ilverify       Verify emitted IL using the 'ilverify' tool");
    Console.WriteLine("  --ilverify-path <path>");
    Console.WriteLine("                    Path to the ilverify executable when not on PATH");
    Console.WriteLine("  --run             Execute the produced assembly after a successful compilation (console apps only)");
    Console.WriteLine("  -h, --help         Display help");
    Console.WriteLine();
    Console.WriteLine("Init command:");
    Console.WriteLine("  init              Create a .ravenproj project scaffold in the current directory.");
    Console.WriteLine("  init --name <name>");
    Console.WriteLine("                    Override the generated project/assembly name.");
    Console.WriteLine("  init --framework <tfm>");
    Console.WriteLine("                    Set TargetFramework in the generated project file.");
    Console.WriteLine("  init --type <app|classlib>");
    Console.WriteLine("                    Set OutputKind in the generated project file (default: app).");
    Console.WriteLine("  init --force      Overwrite existing scaffold files.");
}

static void RunAssembly(string outputFilePath)
{
    using var process = Process.Start(new ProcessStartInfo
    {
        FileName = "dotnet",
        ArgumentList = { outputFilePath },
        UseShellExecute = false,
    });

    if (process is null)
    {
        AnsiConsole.MarkupLine("[red]Failed to start the produced assembly.[/]");
        Environment.ExitCode = 1;
        return;
    }

    process.WaitForExit();
    Environment.ExitCode = process.ExitCode;
}

static bool TryParseSyntaxTreeFormat(string[] args, ref int index, out SyntaxTreeFormat format)
{
    var value = ConsumeOptionValue(args, ref index);
    if (value is null)
    {
        format = SyntaxTreeFormat.Flat;
        return true;
    }

    switch (value.ToLowerInvariant())
    {
        case "group":
        case "grouped":
            format = SyntaxTreeFormat.Group;
            return true;
        case "flat":
            format = SyntaxTreeFormat.Flat;
            return true;
    }

    AnsiConsole.MarkupLine($"[red]Unknown syntax tree format '{value}'.[/]");
    format = SyntaxTreeFormat.Flat;
    return false;
}

static int RunInitCommand(string[] args)
{
    string? name = null;
    var framework = TargetFrameworkUtil.GetLatestFramework();
    var isClassLibrary = false;
    var force = false;

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
                    AnsiConsole.MarkupLine("[red]Missing value for --name.[/]");
                    PrintInitHelp();
                    return 1;
                }

                name = args[++i];
                break;
            case "--framework":
                if (i + 1 >= args.Length)
                {
                    AnsiConsole.MarkupLine("[red]Missing value for --framework.[/]");
                    PrintInitHelp();
                    return 1;
                }

                framework = args[++i];
                break;
            case "--type":
                if (i + 1 >= args.Length)
                {
                    AnsiConsole.MarkupLine("[red]Missing value for --type.[/]");
                    PrintInitHelp();
                    return 1;
                }

                var typeValue = args[++i];
                if (string.Equals(typeValue, "app", StringComparison.OrdinalIgnoreCase))
                {
                    isClassLibrary = false;
                }
                else if (string.Equals(typeValue, "classlib", StringComparison.OrdinalIgnoreCase))
                {
                    isClassLibrary = true;
                }
                else
                {
                    AnsiConsole.MarkupLine($"[red]Invalid --type '{Markup.Escape(typeValue)}'. Use 'app' or 'classlib'.[/]");
                    PrintInitHelp();
                    return 1;
                }

                break;
            case "--force":
                force = true;
                break;
            default:
                AnsiConsole.MarkupLine($"[red]Unknown init option '{Markup.Escape(args[i])}'.[/]");
                PrintInitHelp();
                return 1;
        }
    }

    var cwd = Directory.GetCurrentDirectory();
    var fallbackName = Path.GetFileName(cwd.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar));
    var projectName = SanitizeProjectName(string.IsNullOrWhiteSpace(name) ? fallbackName : name!);
    var projectFilePath = Path.Combine(cwd, $"{projectName}.ravenproj");
    var srcDir = Path.Combine(cwd, "src");
    var mainSourcePath = Path.Combine(srcDir, "main.rav");
    var binDir = Path.Combine(cwd, "bin");
    var binGitkeep = Path.Combine(binDir, ".gitkeep");
    var outputKind = isClassLibrary
        ? OutputKind.DynamicallyLinkedLibrary
        : OutputKind.ConsoleApplication;

    if (!force)
    {
        var existing = new[] { projectFilePath, mainSourcePath, binGitkeep }.Where(File.Exists).ToArray();
        if (existing.Length > 0)
        {
            AnsiConsole.MarkupLine("[red]Init aborted: one or more scaffold files already exist.[/]");
            foreach (var file in existing)
                AnsiConsole.MarkupLine($"[grey]- {Markup.Escape(file)}[/]");
            AnsiConsole.MarkupLine("[grey]Use --force to overwrite scaffold files.[/]");
            return 1;
        }
    }

    Directory.CreateDirectory(srcDir);
    Directory.CreateDirectory(binDir);

    var projectXml = $"""
                      <Project Name="{projectName}" TargetFramework="{framework}" Output="{projectName}" OutputKind="{outputKind}">
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

    AnsiConsole.MarkupLine("[green]Raven project scaffold created.[/]");
    AnsiConsole.MarkupLine($"[grey]- {Markup.Escape(projectFilePath)}[/]");
    AnsiConsole.MarkupLine($"[grey]- {Markup.Escape(mainSourcePath)}[/]");
    AnsiConsole.MarkupLine($"[grey]- {Markup.Escape(binGitkeep)}[/]");
    AnsiConsole.MarkupLine("[grey]Compile with: ravenc ./" + Markup.Escape(Path.GetFileName(projectFilePath)) + " -o bin/" + Markup.Escape(projectName) + ".dll[/]");

    return 0;
}

static void PrintInitHelp()
{
    Console.WriteLine("Usage: ravenc init [--name <project-name>] [--framework <tfm>] [--type <app|classlib>] [--force]");
    Console.WriteLine();
    Console.WriteLine("Creates a Raven project scaffold in the current directory:");
    Console.WriteLine("  - <project-name>.ravenproj");
    Console.WriteLine("  - src/main.rav");
    Console.WriteLine("  - bin/.gitkeep");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("  --name <project-name>      Override generated project/assembly name.");
    Console.WriteLine("  --framework <tfm>          Set TargetFramework (default: latest installed).");
    Console.WriteLine("  --type <app|classlib>      Set OutputKind (default: app).");
    Console.WriteLine("  --force                    Overwrite scaffold files.");
}

static string SanitizeProjectName(string name)
{
    if (string.IsNullOrWhiteSpace(name))
        return "RavenApp";

    var invalid = Path.GetInvalidFileNameChars();
    var cleaned = new string(name.Where(ch => !invalid.Contains(ch)).ToArray()).Trim();
    return string.IsNullOrWhiteSpace(cleaned) ? "RavenApp" : cleaned;
}

static bool TryParseSyntaxDumpFormat(string[] args, ref int index, out bool printRawSyntax, out bool printSyntax,
    out bool includeDiagnostics)
{
    printRawSyntax = false;
    printSyntax = false;
    includeDiagnostics = true;

    var value = ConsumeOptionValue(args, ref index);
    if (value is null)
    {
        printRawSyntax = true;
        return true;
    }

    var segments = value.Split(':', StringSplitOptions.RemoveEmptyEntries);
    if (segments.Length == 0)
    {
        printRawSyntax = true;
        return true;
    }

    switch (segments[0].ToLowerInvariant())
    {
        case "plain":
            if (segments.Length > 1)
            {
                AnsiConsole.MarkupLine("[red]The 'plain' syntax dump does not accept modifiers.[/]");
                return false;
            }

            printRawSyntax = true;
            return true;

        case "pretty":
            includeDiagnostics = true;
            foreach (var modifier in segments.Skip(1))
            {
                switch (modifier.ToLowerInvariant())
                {
                    case "no-diagnostics":
                    case "no-underline":
                        includeDiagnostics = false;
                        break;
                    case "diagnostics":
                    case "underline":
                        includeDiagnostics = true;
                        break;
                    default:
                        AnsiConsole.MarkupLine($"[red]Unknown modifier '{modifier}' for pretty syntax dump.[/]");
                        return false;
                }
            }

            printSyntax = true;
            return true;
    }

    AnsiConsole.MarkupLine($"[red]Unknown syntax dump format '{value}'.[/]");
    return false;
}

static bool TryParseOutputKind(string[] args, ref int index, out OutputKind outputKind)
{
    var value = ConsumeOptionValue(args, ref index);

    if (value is null)
    {
        outputKind = OutputKind.ConsoleApplication;
        return true;
    }

    switch (value.ToLowerInvariant())
    {
        case "console":
        case "consoleapp":
        case "console-app":
        case "exe":
            outputKind = OutputKind.ConsoleApplication;
            return true;

        case "classlib":
        case "class-library":
        case "classlibrary":
        case "library":
        case "dll":
            outputKind = OutputKind.DynamicallyLinkedLibrary;
            return true;
    }

    AnsiConsole.MarkupLine($"[red]Unknown output type '{value}'.[/]");
    outputKind = OutputKind.ConsoleApplication;
    return false;
}

static bool TryParseDocumentationFormat(string[] args, ref int index, out DocumentationFormat format)
{
    var value = ConsumeOptionValue(args, ref index);

    if (value is null)
    {
        format = DocumentationFormat.Markdown;
        return true;
    }

    switch (value.ToLowerInvariant())
    {
        case "md":
        case "markdown":
            format = DocumentationFormat.Markdown;
            return true;
        case "xml":
            format = DocumentationFormat.Xml;
            return true;
    }

    AnsiConsole.MarkupLine($"[red]Unknown documentation format '{value}'.[/]");
    format = DocumentationFormat.Markdown;
    return false;
}

static bool TryParseDocumentationTool(string[] args, ref int index, out DocumentationTool tool)
{
    var value = ConsumeOptionValue(args, ref index);

    if (value is null)
    {
        tool = DocumentationTool.RavenDoc;
        return true;
    }

    switch (value.ToLowerInvariant())
    {
        case "ravendoc":
        case "doc":
        case "docs":
            tool = DocumentationTool.RavenDoc;
            return true;
        case "comments":
        case "comment":
        case "emitted":
            tool = DocumentationTool.CommentEmitter;
            return true;
    }

    AnsiConsole.MarkupLine($"[red]Unknown documentation tool '{value}'.[/]");
    tool = DocumentationTool.RavenDoc;
    return false;
}

static string? ConsumeOptionValue(string[] args, ref int index)
{
    if (index + 1 < args.Length)
    {
        var candidate = args[index + 1];
        if (!candidate.StartsWith('-'))
        {
            index++;
            return candidate;
        }
    }

    return null;
}

static bool TryParseSymbolDumpMode(string[] args, ref int index, out SymbolDumpMode mode)
{
    var value = ConsumeOptionValue(args, ref index);

    if (value is null)
    {
        mode = SymbolDumpMode.List;
        return true;
    }

    switch (value.ToLowerInvariant())
    {
        case "list":
            mode = SymbolDumpMode.List;
            return true;
        case "hierarchy":
            mode = SymbolDumpMode.Hierarchy;
            return true;
    }

    AnsiConsole.MarkupLine($"[red]Unknown symbol dump format '{value}'.[/]");
    mode = SymbolDumpMode.None;
    return false;
}

static bool TryParseNonNegativeInt(string[] args, ref int index, out int value)
{
    value = 0;

    if (index + 1 >= args.Length)
        return false;

    if (!int.TryParse(args[++index], out value))
        return false;

    if (value < 0)
        return false;

    return true;
}

enum SyntaxTreeFormat
{
    Flat,
    Group
}

enum SymbolDumpMode
{
    None,
    List,
    Hierarchy
}

enum DocumentationTool
{
    RavenDoc,
    CommentEmitter
}

readonly record struct CompilerExecutionOptions(
    OutputKind OutputKind,
    bool AllowUnsafe,
    bool EnableAsyncInvestigation,
    string AsyncInvestigationLabel,
    AsyncInvestigationPointerLabelScope AsyncInvestigationScope,
    bool EmbedCoreTypes,
    bool EnableOverloadLog,
    string? OverloadLogPath);
