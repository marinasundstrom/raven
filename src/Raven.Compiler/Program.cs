using System.Diagnostics;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Spectre.Console;

using static Raven.AppHostBuilder;
using static Raven.ConsoleEx;

// ONLY FOR DEVELOPMENT
bool debug = true;

var stopwatch = Stopwatch.StartNew();

// ravc test.rav [-o test.exe]
// dotnet run -- test.rav [-o test.exe]

// Options:
// -s - display the syntax tree
// -d - dump syntax (highlighted)

var filePath = args.Length > 0 ? args[0] : $"../../../samples/classes{RavenFileExtensions.Raven}";
var outputPath = args.Contains("-o") ? args[Array.IndexOf(args, "-o") + 1] : "test.dll"; //: null;

var shouldPrintSyntaxTree = args.Contains("-s");
var shouldDumpSyntax = args.Contains("-d");

var shouldDumpRawSyntax = false;
var shouldDumpBinders = false;

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

//var block = root.DescendantNodes().OfType<BlockSyntax>().ElementAt(2);
//var props = block?.GetChildrenGroupedByProperty(true);

var assemblyName = Path.GetFileNameWithoutExtension(filePath);

var targetFramework = "net9.0";
var tfm = TargetFrameworkMoniker.ToTfm(targetFramework);
var options = new CompilationOptions(OutputKind.ConsoleApplication);
var refAssembliesPath = ReferenceAssemblyPaths.GetReferenceAssemblyDir(targetFramework: tfm);

var compilation = Compilation.Create(assemblyName, options)
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

EmitResult? result = null;

//try
//{
using (var stream = File.OpenWrite($"{outputPath}"))
{
    result = compilation.Emit(stream);
}
//}
/*catch (Exception e)
{
    Console.WriteLine($"Code generation failed: {e}");

    //return;
} */

stopwatch.Stop();

if (shouldDumpRawSyntax || debug)
{
    var str = root.ToFullString();
    Console.WriteLine(str);

    Console.WriteLine();
}

if (shouldPrintSyntaxTree || debug)
{
    var includeLocations = true;
    root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = includeLocations, Colorize = true, ExpandListsAsProperties = true });
}

if (shouldDumpSyntax || debug)
{
    ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

    Console.WriteLine(root.WriteNodeToText(compilation));

    Console.WriteLine();
}

if (shouldDumpBinders || debug)
{
    var semanticModel = compilation.GetSemanticModel(syntaxTree);
    semanticModel.PrintBinderTree();

    Console.WriteLine();
}

if (result is not null)
{
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

        CreateAppHost(compilation, outputPath, targetFramework);
    }
}

//Console.WriteLine(compilation.GlobalNamespace.ToSymbolHierarchyString());

static bool IsProjectFolder(string path)
{
    return Directory.EnumerateFiles(path, "*.csproj", SearchOption.TopDirectoryOnly).Any();
}
