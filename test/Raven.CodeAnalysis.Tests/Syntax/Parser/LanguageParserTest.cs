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
    public void ParseForInExpression_WithExplicitBindingKeyword()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for val x in arr {
                       x
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        var target = Assert.IsType<IdentifierNameSyntax>(forStmt.Target);
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
    public void ParseForWithDiscardTarget_AndOuterBindingKeyword()
    {
        var code = """
                   val arr = [1, 2, 3];
                   for val _ in arr {
                       0
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        Assert.IsType<DiscardPatternSyntax>(forStmt.Target);
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

    [Fact]
    public void ParseForPatternTarget_WithOuterBindingKeyword()
    {
        var code = """
                   val persons = [Person(1, "Ada", 20)];
                   for val Person(1, name, _) in persons {
                       name
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        forStmt!.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        forStmt.Target.ShouldBeOfType<NominalDeconstructionPatternSyntax>();
    }

    [Fact]
    public void ParseForPatternTarget_WithTrailingWholePatternDesignation()
    {
        var code = """
                   val points = [(2, 1.0)];
                   for val (2, > 0.5) point in points {
                       point
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();
        var pattern = forStmt!.Target.ShouldBeOfType<PositionalPatternSyntax>();
        var designation = pattern.Designation.ShouldBeOfType<SingleVariableDesignationSyntax>();
        designation.Identifier.ValueText.ShouldBe("point");
    }

    [Fact]
    public void ParseForPatternTarget_WithGuardedBinding()
    {
        var code = """
                   val orders = [(1001, 120)];
                   for val (id, amount when > 100) in orders {
                       amount
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var root = syntaxTree.GetRoot();

        var forStmt = root.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault();
        forStmt.ShouldNotBeNull();

        var pattern = forStmt!.Target.ShouldBeOfType<PositionalPatternSyntax>();
        var guarded = pattern.Elements[1].Pattern.ShouldBeOfType<GuardedPatternSyntax>();
        guarded.Pattern.ShouldBeOfType<VariablePatternSyntax>();
        guarded.WhenClause.Guard.ShouldBeOfType<ComparisonPatternSyntax>();
    }
}
