namespace Raven.CodeAnalysis.Syntax.Tests;

public class Sandbox(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test()
    {
        var code = """
                    import System;

                   let foo = 2; // + 3;
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true, false, includeNames: true));
        testOutputHelper.WriteLine(root.ToFullString());

        root.PrintSyntaxTree(includeNames: true, includeTokens: true, includeTrivia: true, includeSpans: false, includeLocation: true);

        #region Compilation

        var refAssembliesPath = ReferenceAssemblyPaths.GetReferenceAssemblyDir();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ])
        .AnalyzeCodeTemp(); // Temporary

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var x = semanticModel.GetSymbolInfo(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First());

        #endregion
    }
}