using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

var fileName = args.Length > 0 ? args[0] : "../../../test.rav";

using var file = File.OpenRead(fileName);

var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText);

var root = syntaxTree.GetRoot();

Console.WriteLine(root.GetSyntaxTreeRepresentation(includeTokens: true, includeTrivia: false, includeSpans: false, includeLocation: true));

Console.WriteLine();

Console.WriteLine(root.ToFullString());

Console.WriteLine();

foreach (var diagnostic in syntaxTree.GetDiagnostics())
{
    Console.WriteLine(diagnostic.ToString());
}

var compilation = Compilation.Create("MyCompilation")
    .AddSyntaxTrees(syntaxTree);

syntaxTree = compilation.SyntaxTrees.First();

var semanticModel = compilation.GetSemanticModel(syntaxTree);

var variableDeclarator = syntaxTree.GetRoot()
    .DescendantNodes()
    .OfType<VariableDeclaratorSyntax>()
    .First();

var loc = variableDeclarator.GetLocation();

var symbol = semanticModel.GetDeclaredSymbol(variableDeclarator) as ILocalSymbol;

var symbol2 = semanticModel.GetSymbolInfo(variableDeclarator).Symbol as ILocalSymbol;

Console.WriteLine();
