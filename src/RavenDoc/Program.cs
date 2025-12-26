using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using static Raven.ConsoleEx;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

using System.Diagnostics;

class Program
{
    private const string TargetFramework = "net9.0";
    private static readonly string outputDir = "_docs";

    static void Main()
    {
        List<SyntaxTree> syntaxTrees = new List<SyntaxTree>();

        string path = "../../samples";
        if (Debugger.IsAttached)
        {
            path = Path.Combine("../../..", path);
        }

        var files = Directory.GetFiles(path, "*.rav").Where(x => !x.Contains("generic-math-error.rav"));

        foreach (var file in files)
        {
            //string sourceCode = File.ReadAllText(file);
            //syntaxTrees.Add(ParseSyntaxTree(sourceCode, filePath: file));
        }

        path = "../Raven.Core";
        if (Debugger.IsAttached)
        {
            path = Path.Combine("../../..", path);
        }

        var files2 = Directory.GetFiles(path, "*.rav");

        foreach (var file in files2)
        {
            string sourceCode = File.ReadAllText(file);
            syntaxTrees.Add(ParseSyntaxTree(sourceCode, filePath: file));
        }

        string assemblyName = "Raven.Core";

        var compilation = Compilation.Create(assemblyName, syntaxTrees.ToArray(),
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var version = TargetFrameworkResolver.ResolveVersion(TargetFramework);
        var refDir = TargetFrameworkResolver.GetDirectoryPath(version);

        var references = new[]
        {
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.dll")),
            MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Collections.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.Extensions.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.IO.FileSystem.dll"))
        };

        compilation = compilation.AddReferences(references);

        var diagnostics = compilation.GetDiagnostics();

        if (diagnostics.Length > 0)
        {
            PrintDiagnostics(diagnostics, compilation);
            Console.WriteLine("Failed to produce docs");
            return;
        }

        DocumentationGenerator.ProcessCompilation(compilation, outputDir);
    }
}
