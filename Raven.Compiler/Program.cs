using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

var filePath = args.Length > 0 ? args[0] : "../../../test.rav";
filePath = Path.GetFullPath(filePath);

using var file = File.OpenRead(filePath);

var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText, filePath: filePath);

var root = syntaxTree.GetRoot();

//Console.WriteLine(root.GetSyntaxTreeRepresentation(includeTrivia: true, includeSpans: true, includeLocation: true));

//Console.WriteLine();

//Console.WriteLine(root.ToFullString());

//Console.WriteLine();

var name = Path.GetFileNameWithoutExtension(filePath);

var compilation = Compilation.Create(name) // new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        //MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)
    ])
    .ProcessSymbolsTemp(); // Temp

syntaxTree = compilation.SyntaxTrees.First();

//ListNamespaces(compilation);

PrintDiagnostics(compilation.GetDiagnostics());

//GetSymbols(syntaxTree, semanticModel);

// INFO: The sample will compile, but not all constructs are supported yet.
using var stream = File.OpenWrite("MyAssembly.exe");
compilation.Emit(stream);

Console.WriteLine();

static void PrintDiagnostics(IEnumerable<Diagnostic> diagnostics)
{
    foreach (var diagnostic in diagnostics)
    {
        var descriptor = diagnostic.Descriptor;
        var location = diagnostic.Location.GetLineSpan();

        Console.WriteLine($"{Path.GetRelativePath(Environment.CurrentDirectory, location.Path)}({(location.StartLinePosition.Line + 1)},{(location.StartLinePosition.Character + 1)}): {diagnostic}");
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

static void GetSymbols(SyntaxTree syntaxTree, SemanticModel semanticModel)
{
    var variableDeclarator = syntaxTree.GetRoot()
        .DescendantNodes()
        .OfType<VariableDeclaratorSyntax>()
        .First();

    var loc = variableDeclarator.GetLocation();

    var symbol = semanticModel.GetSymbolInfo(variableDeclarator).Symbol as ILocalSymbol;

    //Console.WriteLine(symbol!.ContainingSymbol!.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
}