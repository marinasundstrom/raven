using System.Text;

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

        var str = syntaxTree.GetRoot().ToFullString();

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

        var str = syntaxTree.GetRoot().ToFullString();

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

        var root = syntaxTree.GetRoot();

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
            testOutputHelper.WriteLine(
                $"{diagnostic.Descriptor.Id}: {diagnostic.Descriptor.Title} [{diagnostic.Location.Span}]");
        }

        var root = syntaxTree.GetRoot();

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
            testOutputHelper.WriteLine(
                $"{diagnostic.Descriptor.Id}: {diagnostic.Descriptor.Title} [{diagnostic.Location.Span}]");
        }

        var root = syntaxTree.GetRoot();

        var x = root.DescendantNodes().OfType<LiteralExpressionSyntax>().First();

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
            testOutputHelper.WriteLine(
                $"{diagnostic.Descriptor.Id}: {diagnostic.Descriptor.Title} [{diagnostic.Location.Span}]");
        }

        var root = syntaxTree.GetRoot();

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

        var root = syntaxTree.GetRoot();

        var found = syntaxTree.GetNodeForSpan(new TextSpan(5, 2));

        root = (CompilationUnitSyntax)root.ReplaceNode(found!, IdentifierName("c"));

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(true));
    }

    [Fact]
    public void FindNodeBySpanAndReplace2()
    {
        var originalText = SourceText.From("Hello, World!");
        var updatedText = originalText.WithChange(7, "Universe");

        var changes = updatedText.GetTextChanges(originalText);

        foreach (var change in changes)
        {
            testOutputHelper.WriteLine(change.ToString());
        }
    }

    [Fact]
    public void FindNodeBySpanAndReplace3()
    {
        var originalText = SourceText.From("Hello, Foo!");

        var textChange = new TextChange(
            new TextSpan(2, 9), // Span of the text to replace
            "MyUpdatedMethod" // New text to insert
        );

        var updatedText = originalText.WithChange(textChange);

        var changes = updatedText.GetTextChanges(originalText);

        foreach (var change in changes)
        {
            testOutputHelper.WriteLine(change.ToString());
        }
    }
}