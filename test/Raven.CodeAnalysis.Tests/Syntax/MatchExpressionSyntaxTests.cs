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
    public void MatchExpression_WithOuterValSequencePatternArm_ParsesBindingKeyword()
    {
        var (arm, tree) = ParseFirstMatchArm("val [first, second, ...rest]");

        Assert.Equal(SyntaxKind.ValKeyword, arm.BindingKeyword.Kind);
        var sequence = Assert.IsType<SequencePatternSyntax>(arm.Pattern);
        Assert.Equal(3, sequence.Elements.Count);

        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithOuterValNominalPatternArm_ParsesBindingKeyword()
    {
        var (arm, tree) = ParseFirstMatchArm("val Some((x, y))");

        Assert.Equal(SyntaxKind.ValKeyword, arm.BindingKeyword.Kind);
        Assert.IsType<NominalDeconstructionPatternSyntax>(arm.Pattern);

        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithTrailingWholePatternDesignation_Parses()
    {
        var (arm, tree) = ParseFirstMatchArm("val Some((x, y)) pair");

        Assert.Equal(SyntaxKind.ValKeyword, arm.BindingKeyword.Kind);
        var pattern = Assert.IsType<NominalDeconstructionPatternSyntax>(arm.Pattern);
        var designation = Assert.IsType<SingleVariableDesignationSyntax>(pattern.Designation);
        Assert.Equal("pair", designation.Identifier.ValueText);

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
    public void MatchExpression_WithLeadingDotPatternsOnSeparateLines_ParsesArms()
    {
        const string code = """
let result = token match {
    .Identifier(val text) => text
    .Number(val value) => value.ToString()
    _ => ""
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(3, match.Arms.Count);
        Assert.All(match.Arms, arm => Assert.False(arm.Expression.IsMissing));
        Assert.IsType<MemberPatternSyntax>(match.Arms[0].Pattern);
        Assert.IsType<MemberPatternSyntax>(match.Arms[1].Pattern);
        Assert.IsType<DiscardPatternSyntax>(match.Arms[2].Pattern);
        AssertNoErrors(tree);
    }

    [Fact]
    public void MatchExpression_WithArmExpressionOnFollowingLine_ParsesArms()
    {
        const string code = """
union Status {
    case A(value: int)
    case B(value: int)
}

let status: Status = .A(value: 1)

let result = status match {
    .A(val value) =>
        value
    .B(val value) =>
        value
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);
        Assert.All(match.Arms, arm => Assert.False(arm.Expression.IsMissing));
        AssertNoErrors(tree);
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

    [Fact]
    public void MatchExpression_UnterminatedStringArm_RecoversFollowingArmAndLaterUnion()
    {
        const string code = """
func Main() {
    val value = 1

    val result = value match {
        1 => WriteLine("oops)
        _ => 0
    }
}

union Result(int)
""";

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var match = root.DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var union = root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);
        Assert.IsType<DiscardPatternSyntax>(match.Arms[1].Pattern);
        Assert.Equal("Result", union.Identifier.ValueText);
    }

    [Fact]
    public void MatchExpression_UnterminatedInterpolatedStringShorthandArm_StopsAtEndOfLine()
    {
        const string code = """
func Main() {
    val x2 = MyResult(42)

    match x2 {
        string str => WriteLine("Str: $str")
        int no => WriteLine("No: $no)
    }
}

union MyResult(string | int)
""";

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var match = root.DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var union = root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var secondInvocation = Assert.IsType<InvocationExpressionSyntax>(match.Arms[1].Expression);

        Assert.Equal(2, match.Arms.Count);
        Assert.Empty(secondInvocation.ArgumentList.Arguments);
        Assert.True(secondInvocation.ArgumentList.CloseParenToken.IsMissing);
        Assert.Equal("MyResult", union.Identifier.ValueText);
    }

    [Fact]
    public void MatchExpression_WithBlockExpressionArms_ParsesArmExpressionsAsBlocks()
    {
        const string code = """
let result = 1 match {
    1 => {
        let x = 40
        x + 2
    }
    _ => {
        0
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);
        Assert.IsType<BlockSyntax>(match.Arms[0].Expression);
        Assert.IsType<BlockSyntax>(match.Arms[1].Expression);
    }

    [Fact]
    public void MatchExpression_WithSelfArmExpression_ParsesSelfExpression()
    {
        const string code = """
class C {
    func F() -> C {
        1 match {
            1 => self
            _ => self
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(2, match.Arms.Count);
        Assert.All(match.Arms, arm => Assert.IsType<SelfExpressionSyntax>(arm.Expression));
        AssertNoErrors(tree);
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
