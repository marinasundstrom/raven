using Raven;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Spectre.Console;

using static Raven.AppHostBuilder;

// ravc test.rav [-o test.exe]
// dotnet run -- test.rav [-o test.exe]

var filePath = args.Length > 0 ? args[0] : "../../../samples/io.rav";
var outputPath = args.Contains("-o") ? args[Array.IndexOf(args, "-o") + 1] : "test.dll"; //: null;

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

root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true });

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

var semanticModel = compilation.GetSemanticModel(syntaxTree);

var methodSymbol = semanticModel.GetDeclaredSymbol(root) as IMethodSymbol;
var typeSymbol = methodSymbol?.ContainingType;

//var local = semanticModel.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First()) as ILocalSymbol;

/*
var result1 = semanticModel.AnalyzeControlFlow(root.DescendantNodes().OfType<ExpressionStatementSyntax>().ElementAt(2));
var result2 = semanticModel.AnalyzeDataFlow(root.DescendantNodes().OfType<BlockSyntax>().First());
var result3 = semanticModel.AnalyzeDataFlow(root.DescendantNodes().OfType<AssignmentExpressionSyntax>().Last());
*/

//var service = new CompletionService();
//var items = service.GetCompletions(compilation, syntaxTree, 28);

ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

Console.WriteLine(root.WriteNodeToText(compilation));

Console.WriteLine();

outputPath = !string.IsNullOrEmpty(outputPath) ? outputPath : compilation.AssemblyName;
outputPath = !Path.HasExtension(outputPath) ? $"{outputPath}.dll" : outputPath;

// INFO: The sample will compile, but not all constructs are supported yet.
using (var stream = File.OpenWrite($"{outputPath}"))
{
    var result = compilation.Emit(stream);
    result.WriteToConsole();
}

//CreateAppHost(compilation);

//Console.WriteLine(compilation.GlobalNamespace.ToSymbolHierarchyString());

Console.WriteLine();

static bool IsProjectFolder(string path)
{
    return Directory.EnumerateFiles(path, "*.csproj", SearchOption.TopDirectoryOnly).Any();
}