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

        while i < list.Length {
            let x = list[i];
            stringBuilder.AppendLine(x.ToString());
            if x > 3 {
                Console.WriteLine("Hello, World!");   
            }
            i = i + 1;
        }

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

        var methodSymbol = semanticModel.GetDeclaredSymbol(root) as IMethodSymbol;
        var typeSymbol = methodSymbol?.ContainingType;

        var local = semanticModel.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First());
        
        var method = semanticModel.GetSymbolInfo(root.DescendantNodes().OfType<InvocationExpressionSyntax>().First());
        
        var visitor = new TestSymbolVisitor(compilation);
        visitor.Visit(compilation.GlobalNamespace);

        testOutputHelper.WriteLine(method.Symbol?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var diagnostics = semanticModel.GetDiagnostics();

        testOutputHelper.WriteLine("");

        foreach (var diagnostic in compilation.GetDiagnostics())
        {
            testOutputHelper.WriteLine(diagnostic.ToString());
        }

        #endregion
    }
}

public class TestSymbolVisitor(Compilation compilation) : SymbolVisitor
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
        foreach (var member in node.GetMembers())
        {
            member.Accept(this);
        }
    }

    public override void VisitMethod(IMethodSymbol symbol)
    {
        var syntax = symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();

        //syntax?.PrintSyntaxTree(includeNames: true, includeTokens: true, includeTrivia: true, includeSpans: false, includeLocation: true);

        base.VisitMethod(symbol);
    }
}