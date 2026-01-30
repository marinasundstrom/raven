using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;

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
// --highlight       - display diagnostics with highlighted source
// -h, --help        - display help
// --run             - execute the produced assembly when compilation succeeds (console apps only)

var sourceFiles = new List<string>();
var additionalRefs = new List<string>();
string? targetFrameworkTfm = null;
string? outputPath = null;
var outputKind = OutputKind.ConsoleApplication;
string? ravenCorePath = null;
var ravenCoreExplicitlyProvided = false;
var embedCoreTypes = false;
var skipDefaultRavenCoreLookup = false;

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
var showHelp = false;
var noEmit = false;
var hasInvalidOption = false;
var highlightDiagnostics = false;
var quote = false;
var quoteWithNamedArgs = false;
var runIlVerify = false;
string? ilVerifyPath = null;
var enableAsyncInvestigation = false;
string asyncInvestigationLabel = "Step14";
var asyncInvestigationScope = AsyncInvestigationPointerLabelScope.FieldOnly;
var enableOverloadLog = false;
string? overloadLogPath = null;
var overloadLogOwnsWriter = false;
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
        case "--highlight":
            highlightDiagnostics = true;
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
}

if (sourceFiles.Count == 0)
    sourceFiles.Add($"../../../../../samples/dsl{RavenFileExtensions.Raven}");

if (emitDocs && documentationTool == DocumentationTool.RavenDoc && documentationFormatExplicitlySet &&
    documentationFormat == DocumentationFormat.Xml)
{
    AnsiConsole.MarkupLine("[yellow]RavenDoc only supports Markdown documentation. Ignoring --doc-format xml.[/]");
    documentationFormat = DocumentationFormat.Markdown;
}

if (run && outputKind != OutputKind.ConsoleApplication)
{
    AnsiConsole.MarkupLine("[red]--run is only supported for console applications. Class libraries cannot be executed.[/]");
    Environment.ExitCode = 1;
    return;
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
        if (File.Exists(candidate))
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
}

var defaultAssemblyBaseName = Path.GetFileNameWithoutExtension(sourceFiles[0]);

outputPath = !string.IsNullOrEmpty(outputPath) ? outputPath : defaultAssemblyBaseName;
if (!Path.HasExtension(outputPath))
    outputPath = $"{outputPath}.dll";
var outputFilePath = Path.GetFullPath(outputPath);
var assemblyName = Path.GetFileNameWithoutExtension(outputFilePath);
var outputDirectory = Path.GetDirectoryName(outputFilePath);
if (string.IsNullOrEmpty(outputDirectory))
    outputDirectory = Directory.GetCurrentDirectory();
if (!string.IsNullOrEmpty(ravenCorePath))
{
    Directory.CreateDirectory(outputDirectory!);
    var ravenCoreDestination = Path.Combine(outputDirectory!, Path.GetFileName(ravenCorePath));
    if (!string.Equals(ravenCoreDestination, ravenCorePath, StringComparison.OrdinalIgnoreCase))
    {
        File.Copy(ravenCorePath, ravenCoreDestination, overwrite: true);
        ravenCorePath = ravenCoreDestination;
    }
}

var targetFramework = targetFrameworkTfm ?? TargetFrameworkUtil.GetLatestFramework();
var version = TargetFrameworkResolver.ResolveVersion(targetFramework);

var options = new CompilationOptions(outputKind);
if (enableAsyncInvestigation)
    options = options.WithAsyncInvestigation(
        AsyncInvestigationOptions.Enable(asyncInvestigationLabel, asyncInvestigationScope));
options = options.WithEmbedCoreTypes(embedCoreTypes);
if (enableOverloadLog)
{
    TextWriter writer;
    if (!string.IsNullOrWhiteSpace(overloadLogPath))
    {
        writer = new StreamWriter(File.Open(overloadLogPath, FileMode.Create, FileAccess.Write, FileShare.Read));
        overloadLogOwnsWriter = true;
    }
    else
    {
        writer = Console.Out;
        overloadLogOwnsWriter = false;
    }

    overloadResolutionLog = new OverloadResolutionLog(writer, overloadLogOwnsWriter);
    options = options.WithOverloadResolutionLogger(overloadResolutionLog);
}
var workspace = RavenWorkspace.Create(targetFramework: targetFramework);
workspace.Services.SyntaxTreeProvider.ParseOptions = new ParseOptions
{
    DocumentationMode = true,
    DocumentationFormat = documentationFormat
};
var projectId = workspace.AddProject(assemblyName, compilationOptions: options);
var project = workspace.CurrentSolution.GetProject(projectId)!;

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

if (!string.IsNullOrWhiteSpace(ravenCorePath))
{
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(ravenCorePath));
}

project = project.AddMetadataReference(
    MetadataReference.CreateFromFile(Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "../../../TestDep.dll"))));

foreach (var r in additionalRefs)
{
    var full = Path.GetFullPath(r);
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(full));
}

project = project.AddAnalyzerReference(new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()));
project = project.AddAnalyzerReference(new AnalyzerReference(new EventDelegateMustBeNullableAnalyzer()));
project = project.AddAnalyzerReference(new AnalyzerReference(new VarCanBeValAnalyzer()));
project = project.AddAnalyzerReference(new AnalyzerReference(new MatchExhaustivenessAnalyzer()));
project = project.AddAnalyzerReference(new AnalyzerReference(new PreferValInsteadOfLetAnalyzer()));

workspace.TryApplyChanges(project.Solution);
project = workspace.CurrentSolution.GetProject(projectId)!;

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

var allowConsoleOutput = sourceFiles.Count == 1;
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
            Colorize = true,
            ExpandListsAsProperties = true,
            IncludeDiagnostics = true,
            IncludeAnnotations = true,
            DiagnosticsAsChildren = true,
            AnnotationsAsChildren = true
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
            semanticModel.PrintBinderTree();
            Console.SetOut(original);
            File.WriteAllText(Path.Combine(debugDir, $"{name}.binders.txt"), sw.ToString());
        }

        using (var sw = new StringWriter())
        {
            var original = Console.Out;
            Console.SetOut(sw);
            semanticModel.PrintBoundTree(includeChildPropertyNames: true, groupChildCollections: true);
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
    PrintDiagnostics(diagnostics, compilation, highlightDiagnostics);
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

static void PrintHelp()
{
    Console.WriteLine("Usage: ravenc [options] <source-files>");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("  --framework <tfm>  Target framework (e.g. net8.0)");
    Console.WriteLine("  --refs <path>      Additional metadata reference (repeatable)");
    Console.WriteLine("  --raven-core <path> Reference a prebuilt Raven.Core.dll instead of embedding compiler shims");
    Console.WriteLine("  --emit-core-types-only Embed Raven.Core shims even when Raven.Core.dll is available");
    Console.WriteLine("  --emit-docs       Emit documentation from comments");
    Console.WriteLine("  --output-type <console|classlib>");
    Console.WriteLine("                     Output kind for the produced assembly.");
    Console.WriteLine("  -o <path>          Output assembly path");
    Console.WriteLine("  -s [flat|group]    Display the syntax tree (single file only)");
    Console.WriteLine("                     Use 'group' to display syntax lists grouped by property.");
    Console.WriteLine("  -ps                Print the parsing sequence");
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
    Console.WriteLine("  -q                 Display AST as compilable C# code");
    Console.WriteLine("  --no-emit        Skip emitting the output assembly");
    Console.WriteLine("  --doc-tool [ravendoc|comments]");
    Console.WriteLine("                    Documentation generator to use (default: ravendoc).");
    Console.WriteLine("  --doc-format [md|xml]");
    Console.WriteLine("                    Documentation format for comment emission (default: md).");
    Console.WriteLine("  --ilverify       Verify emitted IL using the 'ilverify' tool");
    Console.WriteLine("  --ilverify-path <path>");
    Console.WriteLine("                    Path to the ilverify executable when not on PATH");
    Console.WriteLine("  --run             Execute the produced assembly after a successful compilation (console apps only)");
    Console.WriteLine("  -h, --help         Display help");
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
