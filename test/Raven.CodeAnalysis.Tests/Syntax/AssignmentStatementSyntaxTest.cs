using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class AssignmentStatementSyntaxTest
{
    [Fact]
    public void ParsesLetElsePatternDeclaration()
    {
        var statement = SyntaxFactory.ParseStatement("let Some(value) = input else { return }");
        var declaration = Assert.IsType<PatternDeclarationAssignmentStatementSyntax>(statement);

        Assert.Equal(SyntaxKind.LetKeyword, declaration.BindingKeyword.Kind);
        Assert.IsType<NominalDeconstructionPatternSyntax>(declaration.Left);
        Assert.NotNull(declaration.ElseClause);
        Assert.IsType<BlockStatementSyntax>(declaration.ElseClause.Statement);
    }

    [Fact]
    public void ParsesAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("x = 1");
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.SimpleAssignmentStatement, assignment.Kind);
        Assert.False(assignment.IsDiscard);
    }

    [Theory]
    [InlineData("x += 1", SyntaxKind.AddAssignmentStatement, SyntaxKind.PlusEqualsToken)]
    [InlineData("x -= 1", SyntaxKind.SubtractAssignmentStatement, SyntaxKind.MinusEqualsToken)]
    [InlineData("x *= 1", SyntaxKind.MultiplyAssignmentStatement, SyntaxKind.StarEqualsToken)]
    [InlineData("x /= 1", SyntaxKind.DivideAssignmentStatement, SyntaxKind.SlashEqualsToken)]
    [InlineData("x &= 1", SyntaxKind.BitwiseAndAssignmentStatement, SyntaxKind.AmpersandEqualsToken)]
    [InlineData("x |= 1", SyntaxKind.BitwiseOrAssignmentStatement, SyntaxKind.BarEqualsToken)]
    [InlineData("x ^= 1", SyntaxKind.BitwiseXorAssignmentStatement, SyntaxKind.CaretEqualsToken)]
    [InlineData("x ??= 1", SyntaxKind.NullCoalesceAssignmentStatement, SyntaxKind.QuestionQuestionEqualsToken)]
    public void ParsesCompoundAssignmentStatements(string source, SyntaxKind expectedKind, SyntaxKind expectedOperator)
    {
        var tree = SyntaxTree.ParseText(source);
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        Assert.Equal(expectedKind, assignment.Kind);
        Assert.Equal(expectedOperator, assignment.OperatorToken.Kind);
    }

    [Fact]
    public void ParsesSequencePatternAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("[val first, val second, _] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.SimpleAssignmentStatement, assignment.Kind);

        var pattern = Assert.IsType<SequencePatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenBracketToken, pattern.OpenBracketToken.Kind);
        Assert.Equal(3, pattern.Elements.Count);
    }

    [Fact]
    public void ParsesSequencePatternDeclarationShorthandAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("val [first, second, _] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);
        var pattern = Assert.IsType<SequencePatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenBracketToken, pattern.OpenBracketToken.Kind);
        Assert.Equal(3, pattern.Elements.Count);

        Assert.True(pattern.Elements[0].Pattern is VariablePatternSyntax or DeclarationPatternSyntax);
        Assert.True(pattern.Elements[1].Pattern is VariablePatternSyntax or DeclarationPatternSyntax);
        Assert.IsType<DiscardPatternSyntax>(pattern.Elements[2].Pattern);
    }

    [Fact]
    public void ParsesSequencePatternDeclarationShorthand_WithImplicitTypedElements()
    {
        var tree = SyntaxTree.ParseText("val [key: string, value: int] = entries");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);
        var pattern = Assert.IsType<SequencePatternSyntax>(assignment.Left);

        Assert.Collection(
            pattern.Elements,
            element =>
            {
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                var single = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);
                Assert.Equal("key", single.Identifier.ValueText);
                Assert.Equal("string", typed.TypeAnnotation.Type.ToString());
            },
            element =>
            {
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                var single = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);
                Assert.Equal("value", single.Identifier.ValueText);
                Assert.Equal("int", typed.TypeAnnotation.Type.ToString());
            });
    }

    [Fact]
    public void ParsesSequencePatternDeclarationShorthand_WithImplicitTypedRestElement()
    {
        var tree = SyntaxTree.ParseText("val [head: string, ..tail: int] = entries");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);
        var pattern = Assert.IsType<SequencePatternSyntax>(assignment.Left);

        var head = Assert.IsType<VariablePatternSyntax>(pattern.Elements[0].Pattern);
        var headType = Assert.IsType<TypedVariableDesignationSyntax>(head.Designation);
        Assert.Equal("string", headType.TypeAnnotation.Type.ToString());

        var restElement = pattern.Elements[1];
        Assert.Equal(SyntaxKind.DotDotToken, restElement.Prefix.DotDotToken.Kind);
        var tail = Assert.IsType<VariablePatternSyntax>(restElement.Pattern);
        var tailType = Assert.IsType<TypedVariableDesignationSyntax>(tail.Designation);
        Assert.Equal("int", tailType.TypeAnnotation.Type.ToString());
    }

    [Fact]
    public void ParsesSequencePatternDeclarationShorthand_WithMiddleRest()
    {
        var tree = SyntaxTree.ParseText("val [first, ..middle, last] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);
        var pattern = Assert.IsType<SequencePatternSyntax>(assignment.Left);
        Assert.Equal(3, pattern.Elements.Count);

        Assert.True(pattern.Elements[0].Pattern is VariablePatternSyntax or DeclarationPatternSyntax);

        var restElement = pattern.Elements[1];
        Assert.Equal(SyntaxKind.DotDotToken, restElement.Prefix.DotDotToken.Kind);
        var middle = Assert.IsType<VariablePatternSyntax>(restElement.Pattern);
        Assert.Equal(SyntaxKind.None, middle.BindingKeyword.Kind);

        Assert.True(pattern.Elements[2].Pattern is VariablePatternSyntax or DeclarationPatternSyntax);
    }

    [Fact]
    public void ParsesSequencePatternDeclarationShorthand_WithFixedSegment()
    {
        var tree = SyntaxTree.ParseText("val [..2 start, tail] = numbers");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);

        var pattern = Assert.IsType<SequencePatternSyntax>(assignment.Left);
        Assert.Equal(2, pattern.Elements.Count);

        var segmentElement = pattern.Elements[0];
        Assert.Equal(SyntaxKind.DotDotToken, segmentElement.Prefix.DotDotToken.Kind);
        Assert.Equal(SyntaxKind.NumericLiteralToken, segmentElement.Prefix.SegmentLengthToken.Kind);

        var start = Assert.IsType<VariablePatternSyntax>(segmentElement.Pattern);
        Assert.Equal(SyntaxKind.None, start.BindingKeyword.Kind);
    }

    [Fact]
    public void ParsesDictionaryPatternDeclarationShorthandAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("val [\"a\": first, \"b\": second] = values");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ValKeyword, assignment.BindingKeyword.Kind);

        var pattern = Assert.IsType<DictionaryPatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenBracketToken, pattern.OpenBracketToken.Kind);
        Assert.Equal(2, pattern.Entries.Count);

        Assert.IsType<LiteralExpressionSyntax>(pattern.Entries[0].Key);
        Assert.True(pattern.Entries[0].Pattern is VariablePatternSyntax or DeclarationPatternSyntax);
        Assert.IsType<LiteralExpressionSyntax>(pattern.Entries[1].Key);
        Assert.True(pattern.Entries[1].Pattern is VariablePatternSyntax or DeclarationPatternSyntax);
    }

    [Fact]
    public void ParsesPositionalPatternDeclarationShorthandAssignmentStatement()
    {
        var tree = SyntaxTree.ParseText("var (first, second, _) = tuple");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.VarKeyword, assignment.BindingKeyword.Kind);

        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.OpenParenToken, pattern.OpenParenToken.Kind);
        Assert.Equal(3, pattern.Elements.Count);
    }

    [Fact]
    public void PositionalPatternDeclarationShorthand_WithNamedImplicitBindings_PreservesElementNames()
    {
        var tree = SyntaxTree.ParseText("val (Items: items, Name: name, Age: age) = person");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);

        Assert.Collection(
            pattern.Elements,
            element =>
            {
                Assert.Equal("Items", element.NameColon?.Name.Identifier.ValueText);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
                Assert.Equal("items", designation.Identifier.ValueText);
            },
            element =>
            {
                Assert.Equal("Name", element.NameColon?.Name.Identifier.ValueText);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
                Assert.Equal("name", designation.Identifier.ValueText);
            },
            element =>
            {
                Assert.Equal("Age", element.NameColon?.Name.Identifier.ValueText);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
                Assert.Equal("age", designation.Identifier.ValueText);
            });
    }

    [Fact]
    public void PositionalPatternDeclarationShorthand_WithImplicitTypedElements_ParsesTypeAnnotations()
    {
        var tree = SyntaxTree.ParseText("val (key: string, value: int) = pair");
        var assignment = tree.GetRoot().DescendantNodes().OfType<PatternDeclarationAssignmentStatementSyntax>().Single();

        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);

        Assert.Collection(
            pattern.Elements,
            element =>
            {
                Assert.Null(element.NameColon);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);
                Assert.Equal("key", designation.Identifier.ValueText);
                Assert.Equal("string", typed.TypeAnnotation.Type.ToString());
            },
            element =>
            {
                Assert.Null(element.NameColon);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);
                Assert.Equal("value", designation.Identifier.ValueText);
                Assert.Equal("int", typed.TypeAnnotation.Type.ToString());
            });
    }

    [Fact]
    public void PositionalPatternAssignment_WithNamedExistingBindings_PreservesElementNames()
    {
        var tree = SyntaxTree.ParseText("(Items: items, Name: name, Age: age) = person");
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        var pattern = Assert.IsType<PositionalPatternSyntax>(assignment.Left);

        Assert.Collection(
            pattern.Elements,
            element =>
            {
                Assert.Equal("Items", element.NameColon?.Name.Identifier.ValueText);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
                Assert.Equal("items", designation.Identifier.ValueText);
            },
            element =>
            {
                Assert.Equal("Name", element.NameColon?.Name.Identifier.ValueText);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
                Assert.Equal("name", designation.Identifier.ValueText);
            },
            element =>
            {
                Assert.Equal("Age", element.NameColon?.Name.Identifier.ValueText);
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var designation = Assert.IsType<SingleVariableDesignationSyntax>(variable.Designation);
                Assert.Equal("age", designation.Identifier.ValueText);
            });
    }
}
