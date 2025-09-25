using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;

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
// -s [flat|group]   - display the syntax tree (single file only)
// -d [plain|pretty[:no-diagnostics]] - dump syntax (single file only)
// -r                - print the source (single file only)
// -b                - print binder tree (single file only)
// --no-emit         - skip emitting the output assembly
// -h, --help        - display help

var sourceFiles = new List<string>();
var additionalRefs = new List<string>();
string? targetFrameworkTfm = null;
string? outputPath = null;

var printSyntaxTree = false;
var syntaxTreeFormat = SyntaxTreeFormat.Flat;
var printSyntax = false;
var printRawSyntax = false;
var prettyIncludeDiagnostics = true;
var printBinders = false;
var showHelp = false;
var noEmit = false;
var hasInvalidOption = false;

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

if (showHelp || hasInvalidOption)
{
    PrintHelp();
    return;
}

if (sourceFiles.Count == 0)
    sourceFiles.Add($"../../../samples/interfaces{RavenFileExtensions.Raven}");

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
        root.PrintSyntaxTree(new PrinterOptions
        {
            IncludeNames = true,
            IncludeTokens = true,
            IncludeTrivia = true,
            IncludeSpans = true,
            IncludeLocations = includeLocations,
            Colorize = true,
            ExpandListsAsProperties = syntaxTreeFormat == SyntaxTreeFormat.Group
        });
    }

    if (printSyntax)
    {
        ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
        Console.WriteLine(root.WriteNodeToText(compilation, includeDiagnostics: prettyIncludeDiagnostics));
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
    Console.WriteLine("  -s [flat|group]    Display the syntax tree (single file only)");
    Console.WriteLine("                     Use 'group' to display syntax lists grouped by property.");
    Console.WriteLine("  -d [plain|pretty[:no-diagnostics]] Dump syntax (single file only)");
    Console.WriteLine("                     'plain' writes the source text, 'pretty' writes highlighted syntax.");
    Console.WriteLine("                     Append ':no-diagnostics' to skip diagnostic underlines when using 'pretty'.");
    Console.WriteLine("  -r                 Print the source (single file only)");
    Console.WriteLine("  -b                 Print binder tree (single file only)");
    Console.WriteLine("  --no-emit        Skip emitting the output assembly");
    Console.WriteLine("  -h, --help         Display help");
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

enum SyntaxTreeFormat
{
    Flat,
    Group
}
