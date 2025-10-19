using System;
using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PatternSyntaxParserTests
{
    [Fact]
    public void DeclarationPattern_WithIdentifier_Parses()
    {
        var (pattern, tree) = ParsePattern("int number");
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text.");

        var declaration = Assert.IsType<DeclarationPatternSyntax>(pattern);
        Assert.Equal("int number", sourceText.ToString(declaration.Span));
        Assert.Equal("int", declaration.Type.ToString());

        var designation = Assert.IsType<SingleVariableDesignationSyntax>(declaration.Designation);
        Assert.Equal("number", designation.Identifier.ValueText);

        AssertNoErrors(tree);
    }

    [Fact]
    public void DeclarationPattern_WithNegativeNumericLiteral_Parses()
    {
        var (pattern, tree) = ParsePattern("-1");
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text.");

        var declaration = Assert.IsType<DeclarationPatternSyntax>(pattern);
        Assert.Equal("-1", sourceText.ToString(declaration.Span));

        var literal = Assert.IsType<LiteralTypeSyntax>(declaration.Type);
        Assert.Equal(SyntaxKind.NumericLiteralType, literal.Kind);
        Assert.Equal("-1", literal.Token.Text);

        AssertNoErrors(tree);
    }

    [Fact]
    public void DiscardPattern_Parses()
    {
        var (pattern, tree) = ParsePattern("_");
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text.");

        var discard = Assert.IsType<DiscardPatternSyntax>(pattern);
        Assert.Equal("_", sourceText.ToString(discard.Span));

        AssertNoErrors(tree);
    }

    [Fact]
    public void VariablePattern_WithTypedParenthesizedDesignation_Parses()
    {
        var (pattern, tree) = ParsePattern("let (first, second): (int, string)");
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text.");

        var variablePattern = Assert.IsType<VariablePatternSyntax>(pattern);
        Assert.Equal("let (first, second): (int, string)", sourceText.ToString(variablePattern.Span));
        Assert.Equal("let", variablePattern.BindingKeyword.Text);

        var typedDesignation = Assert.IsType<TypedVariableDesignationSyntax>(variablePattern.Designation);
        var tupleDesignation = Assert.IsType<ParenthesizedVariableDesignationSyntax>(typedDesignation.Designation);
        Assert.Equal(2, tupleDesignation.Variables.Count);

        Assert.Collection(
            tupleDesignation.Variables,
            variable =>
            {
                var single = Assert.IsType<SingleVariableDesignationSyntax>(variable);
                Assert.Equal("first", single.Identifier.ValueText);
            },
            variable =>
            {
                var single = Assert.IsType<SingleVariableDesignationSyntax>(variable);
                Assert.Equal("second", single.Identifier.ValueText);
            });

        Assert.Equal("(int, string)", typedDesignation.TypeAnnotation.Type.ToString());

        AssertNoErrors(tree);
    }

    [Fact]
    public void UnaryPattern_WithNotKeyword_Parses()
    {
        var (pattern, tree) = ParsePattern("not let value");
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text.");

        var unaryPattern = Assert.IsType<UnaryPatternSyntax>(pattern);
        Assert.Equal("not let value", sourceText.ToString(unaryPattern.Span));
        Assert.Equal("not", unaryPattern.OperatorToken.Text);
        Assert.IsType<VariablePatternSyntax>(unaryPattern.Pattern);

        AssertNoErrors(tree);
    }

    [Fact]
    public void BinaryPattern_WithAndHasHigherPrecedenceThanOr()
    {
        var (pattern, tree) = ParsePattern("let left and let right or let fallback");
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text.");

        var orPattern = Assert.IsType<BinaryPatternSyntax>(pattern);
        Assert.Equal("let left and let right or let fallback", sourceText.ToString(orPattern.Span));
        Assert.Equal(SyntaxKind.OrPattern, orPattern.Kind);
        Assert.Equal("or", orPattern.OperatorToken.Text);

        var andPattern = Assert.IsType<BinaryPatternSyntax>(orPattern.Left);
        Assert.Equal(SyntaxKind.AndPattern, andPattern.Kind);
        Assert.Equal("and", andPattern.OperatorToken.Text);

        var left = Assert.IsType<VariablePatternSyntax>(andPattern.Left);
        var leftDesignation = Assert.IsType<SingleVariableDesignationSyntax>(left.Designation);
        Assert.Equal("left", leftDesignation.Identifier.ValueText);

        var right = Assert.IsType<VariablePatternSyntax>(andPattern.Right);
        var rightDesignation = Assert.IsType<SingleVariableDesignationSyntax>(right.Designation);
        Assert.Equal("right", rightDesignation.Identifier.ValueText);

        var fallback = Assert.IsType<VariablePatternSyntax>(orPattern.Right);
        var fallbackDesignation = Assert.IsType<SingleVariableDesignationSyntax>(fallback.Designation);
        Assert.Equal("fallback", fallbackDesignation.Identifier.ValueText);

        AssertNoErrors(tree);
    }

    private static (PatternSyntax Pattern, SyntaxTree Tree) ParsePattern(string patternText)
    {
        var code = $$"""
let value: object = (1, "two")

let result = value match {
    {{patternText}} => value
    _ => value
}
""";

        var tree = SyntaxTree.ParseText(code);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var sourceText = tree.GetText() ?? throw new InvalidOperationException("Missing source text for syntax tree.");
        var pattern = match.Arms.First().Pattern;
        if (sourceText.ToString(pattern.Span) != patternText)
            throw new InvalidOperationException($"Unable to locate pattern '{patternText}'.");

        return (pattern, tree);
    }

    private static void AssertNoErrors(SyntaxTree tree)
    {
        Assert.DoesNotContain(tree.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
