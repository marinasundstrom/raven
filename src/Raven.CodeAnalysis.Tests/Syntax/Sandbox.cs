using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class Sandbox(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test()
    {
        var code =
        """
        import System;
        import System.Text;

        let list = [1, 42, 3];
        var i = 0; 

        let stringBuilder = new StringBuilder();

        while (i < list.Length) {
            let x = list[i];
            stringBuilder.AppendLine(x.ToString());
            if(x > 3) {
                Console.WriteLine("Hello, World!");   
            }
            i = i + 1;
        };

        Console.WriteLine(stringBuilder.ToString());
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

        var fooSymbol = semanticModel.GetSymbolInfo(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First());
        var consoleWriteLineSymbol = semanticModel.GetSymbolInfo(root.DescendantNodes().OfType<InvocationExpressionSyntax>().First());

        var visitor = new TestSymbolVisitor();
        visitor.VisitNamespace(compilation.GlobalNamespace);

        testOutputHelper.WriteLine(consoleWriteLineSymbol.Symbol?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var diagnostics = semanticModel.GetDiagnostics();

        testOutputHelper.WriteLine("");

        foreach (var diagnostic in compilation.GetDiagnostics())
        {
            testOutputHelper.WriteLine(diagnostic.ToString());
        }

        #endregion
    }
}

public class TestSymbolVisitor : SymbolVisitor
{
    public override void VisitNamespace(INamespaceSymbol node)
    {
        foreach (var member in node.GetMembers())
        {
            member.Accept(this);
        }
    }

    public override void VisitNamedType(INamedTypeSymbol node)
    {
        base.VisitNamedType(node);
    }
}