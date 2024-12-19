using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

var fileName = args.Length > 0 ? args[0] : "../../../test.rav";

using var file = File.OpenRead(fileName);

var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText);

var root = syntaxTree.GetRoot();

Console.WriteLine(root.GetSyntaxTreeRepresentation(includeTrivia: true, includeSpans: true, includeLocation: true));

Console.WriteLine();

Console.WriteLine(root.ToFullString());

Console.WriteLine();

foreach (var diagnostic in syntaxTree.GetDiagnostics())
{
    Console.WriteLine(diagnostic.ToString());
}

var compilation = Compilation.Create("MyCompilation") // new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree);
    /*
    .AddReferences([
        MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)
    ]);
    */

syntaxTree = compilation.SyntaxTrees.First();

var semanticModel = compilation.GetSemanticModel(syntaxTree);

var variableDeclarator = syntaxTree.GetRoot()
    .DescendantNodes()
    .OfType<VariableDeclaratorSyntax>()
    .First();

var loc = variableDeclarator.GetLocation();

var symbol = semanticModel.GetDeclaredSymbol(variableDeclarator) as ILocalSymbol;

var symbol2 = semanticModel.GetSymbolInfo(variableDeclarator).Symbol as ILocalSymbol;

// INFO: This is incomplete and won't work with the sample
// using var stream = File.OpenWrite("MyAssembly.exe");
// compilation.Emit(stream);

Console.WriteLine();