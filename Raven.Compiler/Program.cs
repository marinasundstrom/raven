using Raven;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.AppHostBuilder;

// ravc test.rav
// dotnet run -- test.rav

var filePath = args.Length > 0 ? args[0] : "../../../test.rav";
filePath = Path.GetFullPath(filePath);

using var file = File.OpenRead(filePath);
var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText, filePath: filePath);
var root = syntaxTree.GetRoot();

var assemblyName = Path.GetFileNameWithoutExtension(filePath);

var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        // INFO: Only compiles on Mac. But runs on any platform.
        MetadataReference.CreateFromFile("/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Ref/9.0.0/ref/net9.0/System.Runtime.dll"),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
    ])
    .AnalyzeCodeTemp(); // Temporary

// INFO: The sample will compile, but not all constructs are supported yet.
using (var stream = File.OpenWrite($"{compilation.AssemblyName}.dll"))
{
    var result = compilation.Emit(stream);
    result.Print();
}

CreateAppHost(compilation);