﻿using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

// ravc test.rav
// dotnet run -- test.rav

var filePath = args.Length > 0 ? args[0] : "../../../test.rav";
filePath = Path.GetFullPath(filePath);

using var file = File.OpenRead(filePath);
var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText, filePath: filePath);
var root = syntaxTree.GetRoot();

var compilationName = Path.GetFileNameWithoutExtension(filePath);

var compilation = Compilation.Create(compilationName)
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)
    ])
    .AnalyzeCodeTemp(); // Temporary

// INFO: The sample will compile, but not all constructs are supported yet.
using var stream = File.OpenWrite("MyAssembly.exe");

var result = compilation.Emit(stream);


// Check the result
if (!result.Success)
{
    PrintDiagnostics(result.Diagnostics);

    Console.WriteLine();

    Console.WriteLine($"Build failed with {result.Diagnostics.Count()} error(s)");
}
else
{
    Console.WriteLine("Build succeeded");
}

static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics)
{
    foreach (var diagnostic in diagnostics)
    {
        var descriptor = diagnostic.Descriptor;
        var location = diagnostic.Location.GetLineSpan();

        Console.WriteLine($"{Path.GetRelativePath(Environment.CurrentDirectory, location.Path)}({location.StartLinePosition.Line + 1},{location.StartLinePosition.Character + 1}): {diagnostic}");
    }
}

static void ListNamespaces(Compilation compilation)
{
    var globalNamespace = compilation.GlobalNamespace.GetMembers("System").First() as INamespaceSymbol;

    foreach (var member in globalNamespace!.GetMembers().OfType<INamespaceSymbol>())
    {
        Console.WriteLine(member.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }
}

static void GetSymbol(Compilation compilation)
{
    var syntaxTree = compilation.SyntaxTrees.First();

    var semanticModel = compilation.GetSemanticModel(syntaxTree);

    var variableDeclarator = syntaxTree.GetRoot()
        .DescendantNodes()
        .OfType<VariableDeclaratorSyntax>()
        .First();

    var loc = variableDeclarator.GetLocation();

    var symbol = semanticModel.GetSymbolInfo(variableDeclarator).Symbol as ILocalSymbol;

    Console.WriteLine(symbol!.ContainingSymbol!.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
}