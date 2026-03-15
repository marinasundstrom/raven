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
        var target = Assert.IsType<IdentifierNameSyntax>(forStmt!.Target);
        target.Identifier.Text.ShouldBe("x");
    }

    [Fact]
    public void ParseForInExpression_WithIdentifierTarget()
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
        var target = Assert.IsType<IdentifierNameSyntax>(forStmt!.Target);
        target.Identifier.Text.ShouldBe("x");
    }

    [Fact]
    public void ParseForWithDiscardTarget()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for _ in arr {
                       0
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        Assert.IsType<DiscardPatternSyntax>(forStmt!.Target);
    }

    [Fact]
    public void ParseForWithoutTarget()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for in arr {
                       0
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.Target.ShouldBeNull();
    }

    [Fact]
    public void ParseForRangeWithByClause()
    {
        var code = """
                   for x in 0..10 by 2 {
                       x
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.ByKeyword.Kind.ShouldBe(SyntaxKind.ByKeyword);
        forStmt.StepExpression.ShouldNotBeNull();
    }

    [Fact]
    public void ParseForExclusiveRangeWithByClause()
    {
        var code = """
                   for x in 0..<10 by 2 {
                       x
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.ByKeyword.Kind.ShouldBe(SyntaxKind.ByKeyword);
        forStmt.StepExpression.ShouldNotBeNull();

        var range = Assert.IsType<RangeExpressionSyntax>(forStmt.Expression);
        range.LessThanToken.Kind.ShouldBe(SyntaxKind.LessThanToken);
    }

    [Fact]
    public void ParseAwaitForInExpression()
    {
        var code = """
                   async func Run(values: System.Collections.Generic.IAsyncEnumerable<int>) {
                       await for x in values {
                           x
                       }
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.AwaitKeyword.Kind.ShouldBe(SyntaxKind.AwaitKeyword);
        forStmt.ForKeyword.Kind.ShouldBe(SyntaxKind.ForKeyword);
        var target = Assert.IsType<IdentifierNameSyntax>(forStmt.Target);
        target.Identifier.Text.ShouldBe("x");
    }

    [Fact]
    public void ParseAwaitForInExpression_WithIdentifierTarget()
    {
        var code = """
                   async func Run(values: System.Collections.Generic.IAsyncEnumerable<int>) {
                       await for x in values {
                           x
                       }
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();

        forStmt.ShouldNotBeNull();
        forStmt!.AwaitKeyword.Kind.ShouldBe(SyntaxKind.AwaitKeyword);
        forStmt.ForKeyword.Kind.ShouldBe(SyntaxKind.ForKeyword);
        var target = Assert.IsType<IdentifierNameSyntax>(forStmt.Target);
        target.Identifier.Text.ShouldBe("x");
    }

    [Fact]
    public void ParseForPatternTarget()
    {
        var code = """
                   val points = [(0, 0), (1, 0)];
                   for (val x, 0) in points {
                       x
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.Target.ShouldBeOfType<PositionalPatternSyntax>();
    }
}
