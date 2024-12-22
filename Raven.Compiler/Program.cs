using Raven;
using Raven.CodeAnalysis;
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

result.Print();