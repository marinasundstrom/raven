using System.Diagnostics;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Spectre.Console;

using static Raven.ConsoleEx;
using static Raven.AppHostBuilder;

var stopwatch = Stopwatch.StartNew();

// ravc test.rav [-o test.exe]
// dotnet run -- test.rav [-o test.exe]

// Options:
// -s - display the syntax tree
// -d - dump syntax (highlighted)

var filePath = args.Length > 0 ? args[0] : "../../../samples/tuple.rav";
var outputPath = args.Contains("-o") ? args[Array.IndexOf(args, "-o") + 1] : "test.dll"; //: null;

var shouldPrintSyntaxTree = args.Contains("-s");
var shouldDumpSyntax = args.Contains("-d");

filePath = Path.GetFullPath(filePath);

if (!File.Exists(filePath))
{
    AnsiConsole.MarkupLine($"[red]Input file '{filePath}' doesn't exist.[/]");
    return;
}

using var file = File.OpenRead(filePath);
var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText, filePath: filePath);
var root = syntaxTree.GetRoot();

var assemblyName = Path.GetFileNameWithoutExtension(filePath);

var refAssembliesPath = ReferenceAssemblyPaths.GetReferenceAssemblyDir("9.0.0", "net9.0");

var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
        MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
        MetadataReference.CreateFromFile(IsProjectFolder(Environment.CurrentDirectory) ? "TestDep.dll" : "../../../TestDep.dll")
    ]);

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

// INFO: The sample will compile, but not all constructs are supported yet.

EmitResult? result = null;

using (var stream = File.OpenWrite($"{outputPath}"))
{
    result = compilation.Emit(stream);
}

stopwatch.Stop();

if (shouldPrintSyntaxTree)
{
    root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true });
}

if (shouldDumpSyntax)
{
    ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

    Console.WriteLine(root.WriteNodeToText(compilation));

    Console.WriteLine();
}

// Check the result
if (!result.Success)
{
    PrintDiagnostics(result.Diagnostics);

    Console.WriteLine();

    Failed(result);
}
else
{
    var warningsCount = result.Diagnostics
        .Count(x => x.Descriptor.DefaultSeverity == DiagnosticSeverity.Warning);

    if (warningsCount > 0)
    {
        SucceededWithWarnings(warningsCount, stopwatch.Elapsed);
    }
    else
    {
        Succeeded(stopwatch.Elapsed);
    }
}

//CreateAppHost(compilation);

//Console.WriteLine(compilation.GlobalNamespace.ToSymbolHierarchyString());

static bool IsProjectFolder(string path)
{
    return Directory.EnumerateFiles(path, "*.csproj", SearchOption.TopDirectoryOnly).Any();
}