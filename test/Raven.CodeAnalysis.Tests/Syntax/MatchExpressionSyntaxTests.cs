using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class MatchExpressionSyntaxTests
{
    [Fact]
    public void MatchExpression_WithStringArmOnNewLine_ParsesArms()
    {
        const string code = """
let value = "hi"

let result = value match {
    "hi" => "match"
    _ => "default"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);

        var firstArm = match.Arms[0];
        var firstPatternToken = firstArm.Pattern.GetFirstToken();
        Assert.Equal(SyntaxKind.StringLiteralToken, firstPatternToken.Kind);
        Assert.Equal("\"hi\"", firstPatternToken.Text);

        Assert.False(firstArm.Expression.IsMissing);
        var firstExpressionToken = firstArm.Expression.GetFirstToken();
        Assert.Equal(SyntaxKind.StringLiteralToken, firstExpressionToken.Kind);
        Assert.Equal("\"match\"", firstExpressionToken.Text);

        var secondArm = match.Arms[1];
        var discardPattern = Assert.IsType<DiscardPatternSyntax>(secondArm.Pattern);
        Assert.Equal(SyntaxKind.UnderscoreToken, discardPattern.UnderscoreToken.Kind);
        Assert.Equal("_", discardPattern.UnderscoreToken.Text);

        Assert.False(secondArm.Expression.IsMissing);
        var secondExpressionToken = secondArm.Expression.GetFirstToken();
        Assert.Equal(SyntaxKind.StringLiteralToken, secondExpressionToken.Kind);
        Assert.Equal("\"default\"", secondExpressionToken.Text);
    }

    [Fact]
    public void MatchExpression_WithDeclarationPatternArm_ParsesDeclarationPattern()
    {
        var (arm, tree) = ParseFirstMatchArm("int number");

        var declaration = Assert.IsType<DeclarationPatternSyntax>(arm.Pattern);
        Assert.Equal("int", declaration.Type.ToString());

        var designation = Assert.IsType<SingleVariableDesignationSyntax>(declaration.Designation);
        Assert.Equal("number", designation.Identifier.ValueText);

        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithVariablePatternArm_ParsesVariablePattern()
    {
        var (arm, tree) = ParseFirstMatchArm("let value");

        var variable = Assert.IsType<VariablePatternSyntax>(arm.Pattern);
        Assert.Equal(SyntaxKind.LetKeyword, variable.BindingKeyword.Kind);

        var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
        Assert.Equal("value", designation.Identifier.ValueText);

        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithBinaryPatternArm_ParsesBinaryPattern()
    {
        var (arm, tree) = ParseFirstMatchArm("let left and let right");

        var binary = Assert.IsType<BinaryPatternSyntax>(arm.Pattern);
        Assert.Equal(SyntaxKind.AndPattern, binary.Kind);
        Assert.Equal("and", binary.OperatorToken.Text);

        Assert.IsType<VariablePatternSyntax>(binary.Left);
        Assert.IsType<VariablePatternSyntax>(binary.Right);

        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithUnaryPatternArm_ParsesUnaryPattern()
    {
        var (arm, tree) = ParseFirstMatchArm("not let value");

        var unary = Assert.IsType<UnaryPatternSyntax>(arm.Pattern);
        Assert.Equal(SyntaxKind.NotPattern, unary.Kind);
        Assert.Equal("not", unary.OperatorToken.Text);
        Assert.IsType<VariablePatternSyntax>(unary.Pattern);

        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithCommentedOutCatchAll_ParsesArms()
    {
        const string code = """
let r = x match {
    bool => "foo"
    (a: bool, b: string) => "tuple"
    //_ => "none"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Collection(
            match.Arms,
            first => Assert.IsType<DeclarationPatternSyntax>(first.Pattern),
            second => Assert.IsType<PositionalPatternSyntax>(second.Pattern));
    }

    [Fact]
    public void MatchExpression_WithCommentBetweenArms_ParsesArms()
    {
        const string code = """
let r = x match {
    bool => "foo"
    // comment
    (a: bool, b: string) => "tuple"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Collection(
            match.Arms,
            first => Assert.IsType<DeclarationPatternSyntax>(first.Pattern),
            second => Assert.IsType<PositionalPatternSyntax>(second.Pattern));
    }

    [Fact]
    public void MatchExpression_WithTrailingCommentAfterArmExpression_ParsesArms()
    {
        const string code = """
let r = x match {
    bool => "foo" // comment
    (a: bool, b: string) => "tuple"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Collection(
            match.Arms,
            first => Assert.IsType<DeclarationPatternSyntax>(first.Pattern),
            second => Assert.IsType<PositionalPatternSyntax>(second.Pattern));
    }

    [Fact]
    public void MatchExpression_WithBlankLineBetweenArms_ParsesArms()
    {
        const string code = """
let r = x match {
    bool => "foo"

    (a: bool, b: string) => "tuple"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Collection(
            match.Arms,
            first => Assert.IsType<DeclarationPatternSyntax>(first.Pattern),
            second => Assert.IsType<PositionalPatternSyntax>(second.Pattern));
    }

    [Fact]
    public void MatchExpression_InvalidTokenBetweenArms_RecoversAndParsesFollowingArm()
    {
        const string code = """
let result = value match {
    1 => 1
    )
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.True(match.Arms.Count >= 2);
        Assert.IsType<DiscardPatternSyntax>(match.Arms.Last().Pattern);
    }

    [Fact]
    public void MatchExpression_MissingArmExpression_RecoversAndParsesFollowingArm()
    {
        const string code = """
let result = value match {
    1 =>
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);
        Assert.True(match.Arms[0].Expression.IsMissing);
        Assert.IsType<DiscardPatternSyntax>(match.Arms[1].Pattern);
    }

    private static (MatchArmSyntax Arm, SyntaxTree Tree) ParseFirstMatchArm(string patternText)
    {
        var code = $$"""
let value = 0

let result = value match {
    {{patternText}} => value
    _ => value
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        return (match.Arms[0], tree);
    }

    private static void AssertNoErrors(SyntaxTree tree)
    {
        Assert.DoesNotContain(tree.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
