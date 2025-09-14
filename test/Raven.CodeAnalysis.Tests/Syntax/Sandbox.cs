using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class Sandbox
{
    [Fact]
    public void Test()
    {
        var code =
        """
        import System.*;
        import System.Text.*;

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

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void Test2()
    {
        var code =
        """
        import System.*;

        Console.Wri;
        """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
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
            import System.*;

            Con
            """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();

        var items = service.GetCompletions(compilation, syntaxTree, 18);

        Assert.All(items, i => i.InsertionText.StartsWith("Con"));
    }

    [Fact]
    public void Test4()
    {
        var code =
            """
            import System.*;

            let x = args;
            var i = 0;
            while i < x.Length {
                Console.WriteLine(x[i]);
            }
            """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();

        var items = service.GetCompletions(compilation, syntaxTree, 20);

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
