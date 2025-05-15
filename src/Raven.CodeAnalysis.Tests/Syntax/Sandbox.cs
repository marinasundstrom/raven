using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class Sandbox(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test()
    {
        var code = """
                   import System;
                   let x = [1, 2, 3];
                   Console.WriteLine(x.Length);
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true, false, includeNames: true, includeSpans: false, includeLocation: true));
        testOutputHelper.WriteLine(root.ToFullString());

        root.PrintSyntaxTree(includeNames: true, includeTokens: true, includeTrivia: true, includeSpans: false, includeLocation: true);

        #region Compilation

        var refAssembliesPath = ReferenceAssemblyPaths.GetReferenceAssemblyDir();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        //var fooSymbol = semanticModel.GetSymbolInfo(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First());
        //var consoleWriteLineSymbol = semanticModel.GetSymbolInfo(root.DescendantNodes().OfType<InvocationExpressionSyntax>().First());

        //testOutputHelper.WriteLine(consoleWriteLineSymbol.Symbol?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var diagnostics = semanticModel.GetDiagnostics();

        testOutputHelper.WriteLine("");

        foreach (var diagnostic in compilation.GetDiagnostics())
        {
            testOutputHelper.WriteLine(diagnostic.ToString());
        }

        #endregion
    }
}