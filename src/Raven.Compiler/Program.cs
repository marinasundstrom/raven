using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;

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
// -o <path>         - output assembly path
// -s                - display the syntax tree (single file only)
// -d                - dump syntax with highlighting (single file only)
// -r                - print the source (single file only)
// -b                - print binder tree (single file only)
// --no-emit         - skip emitting the output assembly
// -h, --help        - display help

var sourceFiles = new List<string>();
var additionalRefs = new List<string>();
string? targetFrameworkTfm = null;
string? outputPath = null;

var printSyntaxTree = false;
var printSyntax = false;
var printRawSyntax = false;
var printBinders = false;
var showHelp = false;
var noEmit = false;

for (int i = 0; i < args.Length; i++)
{
    switch (args[i])
    {
        case "-o":
            if (i + 1 < args.Length)
                outputPath = args[++i];
            break;
        case "-s":
            printSyntaxTree = true;
            break;
        case "-d":
            printSyntax = true;
            break;
        case "-r":
            printRawSyntax = true;
            break;
        case "-b":
            printBinders = true;
            break;
        case "--no-emit":
            noEmit = true;
            break;
        case "--ref":
        case "--refs":
            if (i + 1 < args.Length)
                additionalRefs.Add(args[++i]);
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

if (showHelp)
{
    PrintHelp();
    return;
}

if (sourceFiles.Count == 0)
    sourceFiles.Add($"../../../samples/classes{RavenFileExtensions.Raven}");

for (int i = 0; i < sourceFiles.Count; i++)
{
    sourceFiles[i] = Path.GetFullPath(sourceFiles[i]);
    if (!File.Exists(sourceFiles[i]))
    {
        AnsiConsole.MarkupLine($"[red]Input file '{sourceFiles[i]}' doesn't exist.[/]");
        return;
    }
}

var assemblyName = Path.GetFileNameWithoutExtension(sourceFiles[0]);

var targetFramework = targetFrameworkTfm ?? TargetFrameworkUtil.GetLatestFramework();
var version = TargetFrameworkResolver.ResolveVersion(targetFramework);
var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

var options = new CompilationOptions(OutputKind.ConsoleApplication);
var workspace = RavenWorkspace.Create(targetFramework: targetFramework);
var projectId = workspace.AddProject(assemblyName, compilationOptions: options);
var project = workspace.CurrentSolution.GetProject(projectId)!;

foreach (var filePath in sourceFiles)
{
    using var file = File.OpenRead(filePath);
    var sourceText = SourceText.From(file);
    var document = project.AddDocument(Path.GetFileName(filePath), sourceText, filePath);
    project = document.Project;
}

foreach (var reference in new[]
{
    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
    MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
    MetadataReference.CreateFromFile(Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "../../../TestDep.dll")))
})
{
    project = project.AddMetadataReference(reference);
}

foreach (var r in additionalRefs)
{
    var full = Path.GetFullPath(r);
    project = project.AddMetadataReference(MetadataReference.CreateFromFile(full));
}

project = project.AddAnalyzerReference(new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()));
workspace.TryApplyChanges(project.Solution);
project = workspace.CurrentSolution.GetProject(projectId)!;

var compilation = workspace.GetCompilation(projectId);
var diagnostics = workspace.GetDiagnostics(projectId);

outputPath = !string.IsNullOrEmpty(outputPath) ? outputPath : compilation.AssemblyName;
outputPath = !Path.HasExtension(outputPath) ? $"{outputPath}.dll" : outputPath;

EmitResult? result = null;
if (!noEmit)
{
    using (var stream = File.OpenWrite($"{outputPath}"))
    {
        result = compilation.Emit(stream);
    }

    diagnostics = diagnostics.Concat(result!.Diagnostics).Distinct().ToImmutableArray();
}

stopwatch.Stop();

var allowConsoleOutput = sourceFiles.Count == 1;
var debugDir = FindDebugDirectory();

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
            ExpandListsAsProperties = true
        }).StripAnsiCodes();

        File.WriteAllText(Path.Combine(debugDir, $"{name}.syntax-tree.txt"), treeText);

        ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
        var syntax = root.WriteNodeToText(compilation, includeDiagnostics: true).StripAnsiCodes();
        File.WriteAllText(Path.Combine(debugDir, $"{name}.syntax.txt"), syntax);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        using var sw = new StringWriter();
        var original = Console.Out;
        Console.SetOut(sw);
        semanticModel.PrintBinderTree();
        Console.SetOut(original);
        File.WriteAllText(Path.Combine(debugDir, $"{name}.binders.txt"), sw.ToString());
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
        root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = includeLocations, Colorize = true, ExpandListsAsProperties = true });
    }

    if (printSyntax)
    {
        ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
        Console.WriteLine(root.WriteNodeToText(compilation, includeDiagnostics: true));
        Console.WriteLine();
    }

    if (printBinders)
    {
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        semanticModel.PrintBinderTree();
        Console.WriteLine();
    }
}
else if (printRawSyntax || printSyntaxTree || printSyntax || printBinders)
{
    if (debugDir is null)
        AnsiConsole.MarkupLine("[yellow]Create a '.debug' directory to capture debug output.[/]");
}

if (diagnostics.Length > 0)
{
    PrintDiagnostics(diagnostics);
    Console.WriteLine();
}

var errors = diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error && d.Descriptor.Id != "RAV1011");
if (errors.Any())
{
    if (result is not null)
        Failed(result);
    else
        Failed(errors.Count());
}
else
{
    var warningsCount = diagnostics.Count(x => x.Severity == DiagnosticSeverity.Warning);
    if (warningsCount > 0)
        SucceededWithWarnings(warningsCount, stopwatch.Elapsed);
    else
        Succeeded(stopwatch.Elapsed);

    if (result is not null)
        CreateAppHost(compilation, outputPath, targetFramework);
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

static void PrintHelp()
{
    Console.WriteLine("Usage: ravenc [options] <source-files>");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("  --framework <tfm>  Target framework (e.g. net8.0)");
    Console.WriteLine("  --refs <path>      Additional metadata reference (repeatable)");
    Console.WriteLine("  -o <path>          Output assembly path");
    Console.WriteLine("  -s                 Display the syntax tree (single file only)");
    Console.WriteLine("  -d                 Dump syntax with highlighting (single file only)");
    Console.WriteLine("  -r                 Print the source (single file only)");
    Console.WriteLine("  -b                 Print binder tree (single file only)");
    Console.WriteLine("  --no-emit        Skip emitting the output assembly");
    Console.WriteLine("  -h, --help         Display help");
}
