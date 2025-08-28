using System.Diagnostics;
using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Spectre.Console;

using static Raven.AppHostBuilder;
using static Raven.ConsoleEx;

var stopwatch = Stopwatch.StartNew();

// Options:
// -s - display the syntax tree (single file only)
// -d - dump syntax (highlighted)
// -r - print the source
// -b - print binder tree
// --ref <path> - additional metadata reference
// --moniker <tfm> - target framework moniker
// -o <path> - output assembly path

var sourceFiles = new List<string>();
var additionalRefs = new List<string>();
string? moniker = null;
string? outputPath = null;

var shouldPrintSyntaxTree = false;
var shouldDumpSyntax = false;
var shouldDumpRawSyntax = false;
var shouldDumpBinders = false;

for (int i = 0; i < args.Length; i++)
{
    switch (args[i])
    {
        case "-o":
            if (i + 1 < args.Length)
                outputPath = args[++i];
            break;
        case "-s":
            shouldPrintSyntaxTree = true;
            break;
        case "-d":
            shouldDumpSyntax = true;
            break;
        case "-r":
            shouldDumpRawSyntax = true;
            break;
        case "-b":
            shouldDumpBinders = true;
            break;
        case "--ref":
            if (i + 1 < args.Length)
                additionalRefs.Add(args[++i]);
            break;
        case "--moniker":
            if (i + 1 < args.Length)
                moniker = args[++i];
            break;
        default:
            if (!args[i].StartsWith('-'))
                sourceFiles.Add(args[i]);
            break;
    }
}

var debug = DebugFileExists();
shouldPrintSyntaxTree |= debug;
shouldDumpSyntax |= debug;
shouldDumpRawSyntax |= debug;
shouldDumpBinders |= debug;

if (sourceFiles.Count == 0)
    sourceFiles.Add($"../../../samples/main{RavenFileExtensions.Raven}");

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

var targetFramework = moniker ?? TargetFrameworkUtil.GetLatestFramework();
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
    MetadataReference.CreateFromFile(IsProjectFolder(Environment.CurrentDirectory) ? "TestDep.dll" : "../../../TestDep.dll")
})
    project = project.AddMetadataReference(reference);

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

using (var stream = File.OpenWrite($"{outputPath}"))
{
    result = compilation.Emit(stream);
}

diagnostics = diagnostics.Concat(result!.Diagnostics).Distinct().ToImmutableArray();

stopwatch.Stop();

var allowConsoleOutput = sourceFiles.Count == 1;

if (shouldDumpRawSyntax || shouldPrintSyntaxTree || shouldDumpSyntax || shouldDumpBinders)
{
    var debugDir = Path.Combine(Environment.CurrentDirectory, "debug");
    Directory.CreateDirectory(debugDir);

    foreach (var document in project.Documents)
    {
        var syntaxTree = document.GetSyntaxTreeAsync().Result!;
        var root = syntaxTree.GetRoot();
        var name = Path.GetFileNameWithoutExtension(document.FilePath) ?? document.Name;

        if (shouldDumpRawSyntax)
        {
            File.WriteAllText(Path.Combine(debugDir, $"{name}.raw.rav"), root.ToFullString());
        }

        if (shouldPrintSyntaxTree)
        {
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
        }

        if (shouldDumpSyntax)
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
            var syntax = root.WriteNodeToText(compilation, includeDiagnostics: true).StripAnsiCodes();
            File.WriteAllText(Path.Combine(debugDir, $"{name}.syntax.txt"), syntax);
        }

        if (shouldDumpBinders)
        {
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            using var sw = new StringWriter();
            var original = Console.Out;
            Console.SetOut(sw);
            semanticModel.PrintBinderTree();
            Console.SetOut(original);
            File.WriteAllText(Path.Combine(debugDir, $"{name}.binders.txt"), sw.ToString());
        }
    }
}

if (allowConsoleOutput)
{
    var document = project.Documents.Single();
    var syntaxTree = document.GetSyntaxTreeAsync().Result!;
    var root = syntaxTree.GetRoot();

    if (shouldDumpRawSyntax)
    {
        Console.WriteLine(root.ToFullString());
        Console.WriteLine();
    }

    if (shouldPrintSyntaxTree)
    {
        var includeLocations = true;
        root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = includeLocations, Colorize = true, ExpandListsAsProperties = true });
    }

    if (shouldDumpSyntax)
    {
        ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;
        Console.WriteLine(root.WriteNodeToText(compilation, includeDiagnostics: true));
        Console.WriteLine();
    }

    if (shouldDumpBinders)
    {
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        semanticModel.PrintBinderTree();
        Console.WriteLine();
    }
}
else if (shouldDumpRawSyntax || shouldPrintSyntaxTree || shouldDumpSyntax || shouldDumpBinders)
{
    AnsiConsole.MarkupLine("[yellow]Debug output written to 'debug' folder.[/]");
}

if (diagnostics.Length > 0)
{
    PrintDiagnostics(diagnostics);
    Console.WriteLine();
}

if (result is not null)
{
    if (diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error))
    {
        Failed(result);
    }
    else
    {
        var warningsCount = diagnostics.Count(x => x.Severity == DiagnosticSeverity.Warning);
        if (warningsCount > 0)
            SucceededWithWarnings(warningsCount, stopwatch.Elapsed);
        else
            Succeeded(stopwatch.Elapsed);

        CreateAppHost(compilation, outputPath, targetFramework);
    }
}

static bool IsProjectFolder(string path)
{
    return Directory.EnumerateFiles(path, "*.csproj", SearchOption.TopDirectoryOnly).Any();
}

static bool DebugFileExists()
{
    var dir = Environment.CurrentDirectory;
    while (dir is not null)
    {
        var debug = Path.Combine(dir, ".debug");
        if (File.Exists(debug))
            return true;

        dir = Path.GetDirectoryName(dir);
    }

    return false;
}
