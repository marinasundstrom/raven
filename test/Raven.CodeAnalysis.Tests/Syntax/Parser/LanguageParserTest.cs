using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Shouldly;
using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class LanguageParserTest(ITestOutputHelper testOutputHelper)
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

        var root = syntaxTree.GetRoot();

        //var ifExpression = (root.Members.FirstOrDefault() as GlobalStatementSyntax)?.Statement as IfExpressionSyntax;

        var str = syntaxTree.GetRoot().ToFullString();
        testOutputHelper.WriteLine(str);
        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true }));
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

        var root = syntaxTree.GetRoot();

        var str = syntaxTree.GetRoot().ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true }));
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

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true }));
    }

    [Fact]
    public void ParseForInExpression()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for x in arr {
                       x
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.Identifier.Text.ShouldBe("x");
        forStmt.EachKeyword.Kind.ShouldBe(SyntaxKind.None);
    }

    [Fact]
    public void ParseForEachInExpression()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for each x in arr {
                       x
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.Identifier.Text.ShouldBe("x");
        forStmt.EachKeyword.Kind.ShouldBe(SyntaxKind.EachKeyword);
    }

    [Fact]
    public void ParseForEachWithDiscardIdentifier()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for each _ in arr {
                       0
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.Identifier.Kind.ShouldBe(SyntaxKind.UnderscoreToken);
    }

    [Fact]
    public void ParseForEachWithoutIdentifier()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for each in arr {
                       0
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.Identifier.Kind.ShouldBe(SyntaxKind.None);
    }
}
