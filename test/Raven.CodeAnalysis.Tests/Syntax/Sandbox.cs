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

        var visitor = new TestSyntaxVisitor();
        visitor.Visit(root);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(new PrinterOptions
        {
            IncludeNames = true,
            IncludeTokens = true,
            IncludeTrivia = true,
            IncludeSpans = false,
            IncludeLocations = true,
            Colorize = true,
            ExpandListsAsProperties = true
        }));
        
        testOutputHelper.WriteLine(root.ToFullString());

        root.PrintSyntaxTree(new PrinterOptions
        {
            IncludeNames = true,
            IncludeTokens = true,
            IncludeTrivia = true,
            IncludeSpans = false,
            IncludeLocations = true,
            Colorize = true,
            ExpandListsAsProperties = true
        });

        #region Compilation

        var version = TargetFrameworkResolver.GetLatestVersion();
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

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

        var visitor2 = new TestSymbolVisitor();
        visitor2.Visit(compilation.GlobalNamespace);

        testOutputHelper.WriteLine(method.Symbol?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var diagnostics = semanticModel.GetDiagnostics();

        testOutputHelper.WriteLine("");

        foreach (var diagnostic in compilation.GetDiagnostics())
        {
            testOutputHelper.WriteLine(diagnostic.ToString());
        }

        #endregion
    }

    [Fact]
    public void Test2()
    {
        var code =
        """
        import System;

        Console.Wri;
        """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.GetLatestVersion();
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();

        var items = service.GetCompletions(compilation, syntaxTree, 28);

        var items3 = service.GetCompletions(compilation, syntaxTree, 25);

        var items4 = service.GetCompletions(compilation, syntaxTree, 26);
    }

    [Fact]
    public void Test3()
    {
        var code =
            """
            import System;

            Con
            """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.GetLatestVersion();
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();

        var items3 = service.GetCompletions(compilation, syntaxTree, 20);
    }

    [Fact]
    public void Test4()
    {
        var code =
            """
            import System;

            let x = args;
            var i = 0;
            while i < x.Length {
                Console.WriteLine(x[i]);
            }
            """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.GetLatestVersion();
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();

        var items3 = service.GetCompletions(compilation, syntaxTree, 20);

        var x = compilation.Module.GlobalNamespace.LookupType("Program");
    }
}

public class TestSyntaxVisitor : SyntaxVisitor
{
    public override void VisitCompilationUnit(CompilationUnitSyntax node)
    {
        foreach (var member in node.Members)
        {
            member.Accept(this);
        }
    }

    public override void VisitGlobalStatement(GlobalStatementSyntax node)
    {
        base.VisitGlobalStatement(node);
    }
}

public class TestSymbolVisitor : SymbolVisitor
{
    public override void VisitNamespace(INamespaceSymbol symbol)
    {
        foreach (var member in symbol.GetMembers())
        {
            member.Accept(this);
        }
    }

    public override void VisitNamedType(INamedTypeSymbol symbol)
    {
        foreach (var member in symbol.GetMembers())
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