using Raven.CodeAnalysis.Syntax;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Parser.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class ParserTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void ParseIfStatement()
    {
        var code = """
        if (foo)  {
        
            return 0;
        }
        """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var str = syntaxTree.GetSyntaxRoot().ToFullString();

        testOutputHelper.WriteLine(str);
    }

    [Fact]
    public void ParseIfStatementWithElseClause()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else {
                   
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var str = syntaxTree.GetSyntaxRoot().ToFullString();

        testOutputHelper.WriteLine(str);
    }

    [Fact]
    public void ParseIfStatementWithElseIfClause()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else if (bar ) {
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetSyntaxRoot();

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true));
    }

    [Fact]
    public void Diagnostic()
    {
        var code = """
                   let x
                   return 0;
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var diagnostics = syntaxTree.GetDiagnostics();

        foreach (var diagnostic in diagnostics)
        {
            testOutputHelper.WriteLine($"{diagnostic.Descriptor.Id}: {diagnostic.Descriptor.Title} [{diagnostic.Location.Span}]");
        }

        var root = syntaxTree.GetSyntaxRoot();

        var x = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First();

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true));
    }

    [Fact]
    public void Diagnostic2()
    {
        var code = """
                   return 0;
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var diagnostics = syntaxTree.GetDiagnostics();

        foreach (var diagnostic in diagnostics)
        {
            testOutputHelper.WriteLine($"{diagnostic.Descriptor.Id}: {diagnostic.Descriptor.Title} [{diagnostic.Location.Span}]");
        }

        var root = syntaxTree.GetSyntaxRoot();

        var x = root.DescendantNodes().OfType<IdentifierNameSyntax>().First();

        root = (CompilationUnitSyntax)root.ReplaceNode(x, IdentifierName("c"));

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true));
    }
    
    [Fact]
    public void ReplaceNode()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else if (bar ) {
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var diagnostics = syntaxTree.GetDiagnostics();

        foreach (var diagnostic in diagnostics)
        {
            testOutputHelper.WriteLine($"{diagnostic.Descriptor.Id}: {diagnostic.Descriptor.Title} [{diagnostic.Location.Span}]");
        }

        var root = syntaxTree.GetSyntaxRoot();

        var x = root.DescendantNodes().OfType<IdentifierNameSyntax>().First();

        root = (CompilationUnitSyntax)root.ReplaceNode(x, IdentifierName("c"));

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true));
    }
    
    [Fact]
    public void FindNodeBySpanAndReplace()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else if (bar ) {
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetSyntaxRoot();

        var found = syntaxTree.GetNodeForSpan(new TextSpan(5, 2));
        
        root = (CompilationUnitSyntax)root.ReplaceNode(found!, IdentifierName("c"));

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true));
    }
}