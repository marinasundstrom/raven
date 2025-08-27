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

// ravc test.rav [-o test.exe]
// dotnet run -- test.rav [-o test.exe]

// Options:
// -s - display the syntax tree
// -d - dump syntax (highlighted)
// -r - print the source
// -b - print binder tree

var filePath = args.Length > 0 ? args[0] : $"../../../samples/main{RavenFileExtensions.Raven}";
var outputPath = args.Contains("-o") ? args[Array.IndexOf(args, "-o") + 1] : "test.dll"; //: null;

var debug = DebugFileExists();

var shouldPrintSyntaxTree = debug || args.Contains("-s");
var shouldDumpSyntax = debug || args.Contains("-d");
var shouldDumpRawSyntax = debug || args.Contains("-r");
var shouldDumpBinders = debug || args.Contains("-b");

filePath = Path.GetFullPath(filePath);

if (!File.Exists(filePath))
{
    AnsiConsole.MarkupLine($"[red]Input file '{filePath}' doesn't exist.[/]");
    return;
}

using var file = File.OpenRead(filePath);
var sourceText = SourceText.From(file);

var assemblyName = Path.GetFileNameWithoutExtension(filePath);

var targetFramework = "net9.0";

var options = new CompilationOptions(OutputKind.ConsoleApplication);
var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version /* version.Moniker.Version.ToString() + ".*" */);

var workspace = RavenWorkspace.Create(targetFramework: targetFramework);
var projectId = workspace.AddProject(assemblyName, compilationOptions: options);
var documentId = DocumentId.CreateNew(projectId);
var solution = workspace.CurrentSolution.AddDocument(documentId, Path.GetFileName(filePath), sourceText, filePath);
workspace.TryApplyChanges(solution);

var project = workspace.CurrentSolution.GetProject(projectId)!;
foreach (var reference in new[]
{
    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
    MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
    MetadataReference.CreateFromFile(IsProjectFolder(Environment.CurrentDirectory) ? "TestDep.dll" : "../../../TestDep.dll")
})
    project = project.AddMetadataReference(reference);

project = project.AddAnalyzerReference(new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()));
workspace.TryApplyChanges(project.Solution);

var document = project.Documents.Single();
var syntaxTree = document.GetSyntaxTreeAsync().Result!;
var root = syntaxTree.GetRoot();

var compilation = workspace.GetCompilation(projectId);
var diagnostics = workspace.GetDiagnostics(projectId);

//var semanticModel = compilation.GetSemanticModel(syntaxTree);

//var methodSymbol = semanticModel.GetDeclaredSymbol(root) as IMethodSymbol;
//var typeSymbol = methodSymbol?.ContainingType;

//var local = semanticModel.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First()) as ILocalSymbol;

/*
var result1 = semanticModel.AnalyzeControlFlow(root.DescendantNodes().OfType<ExpressionStatementSyntax>().ElementAt(2));
var result2 = semanticModel.AnalyzeDataFlow(root.DescendantNodes().OfType<BlockSyntax>().First());
var result3 = semanticModel.AnalyzeDataFlow(root.DescendantNodes().OfType<AssignmentExpressionSyntax>().Last());
*/

//var service = new CompletionService();
//var items = service.GetCompletions(compilation, syntaxTree, 28);

outputPath = !string.IsNullOrEmpty(outputPath) ? outputPath : compilation.AssemblyName;
outputPath = !Path.HasExtension(outputPath) ? $"{outputPath}.dll" : outputPath;

EmitResult? result = null;

using (var stream = File.OpenWrite($"{outputPath}"))
{
    result = compilation.Emit(stream);
}

diagnostics = diagnostics.Concat(result!.Diagnostics).Distinct().ToImmutableArray();

stopwatch.Stop();

if (shouldDumpRawSyntax)
{
    var str = root.ToFullString();
    Console.WriteLine(str);

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

    Console.WriteLine(root.WriteNodeToText(compilation));

    Console.WriteLine();
}

if (shouldDumpBinders)
{
    var semanticModel = compilation.GetSemanticModel(syntaxTree);
    semanticModel.PrintBinderTree();

    Console.WriteLine();
}

if (diagnostics.Length > 0)
{
    PrintDiagnostics(diagnostics);
    Console.WriteLine();
}

if (result is not null)
{
    // Check the result
    if (diagnostics.Any(d => d.Descriptor.DefaultSeverity == DiagnosticSeverity.Error))
    {
        Failed(result);
    }
    else
    {
        var warningsCount = diagnostics
            .Count(x => x.Descriptor.DefaultSeverity == DiagnosticSeverity.Warning);

        if (warningsCount > 0)
        {
            SucceededWithWarnings(warningsCount, stopwatch.Elapsed);
        }
        else
        {
            Succeeded(stopwatch.Elapsed);
        }

        CreateAppHost(compilation, outputPath, targetFramework);
    }
}

//Console.WriteLine(compilation.GlobalNamespace.ToSymbolHierarchyString());

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
