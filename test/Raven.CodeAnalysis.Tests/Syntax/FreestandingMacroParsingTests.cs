using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class FreestandingMacroParsingTests
{
    [Fact]
    public void FreestandingMacroExpression_ParsesHashIdentifierAndArguments()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => #add(1, right: 2)
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<HashMacroExpressionSyntax>()
            .Single();

        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("add", macroName);
        Assert.Equal(SyntaxKind.HashToken, expression.HashToken.Kind);
        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Equal("right", expression.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void HashDirective_IsNotParsedAsFreestandingMacroExpression()
    {
        var tree = SyntaxTree.ParseText("""
            #pragma warning disable RAV0001
            func Main() -> int => 1
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
    }

    [Fact]
    public void TokenTreeMacroExpression_PreservesRawDslBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => #xml {
                <root data="{not a Raven expression}">
                    {{ nested { content } }}
                </root>
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var tokenTree = Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree);
        Assert.Contains("<root data=\"{not a Raven expression}\">", tokenTree.ToFullString());
        Assert.Contains("{{ nested { content } }}", tokenTree.BodyToken.Text);
        Assert.Equal(SyntaxKind.MacroBodyToken, tokenTree.BodyToken.Kind);
        Assert.False(tokenTree.CloseBraceToken.IsMissing);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_ParsesArgumentsAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => #repeat(3, Label: "item") {
                custom content
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Equal("Label", expression.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Contains("custom content", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_AllowsCharactersOutsideRavenLexicalGrammar()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => #dsl {
                `custom-key` ::= ⟨value⟩
            }
            """);

        var tokenTree = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroTokenTreeSyntax>()
            .Single();

        Assert.Contains("`custom-key` ::= ⟨value⟩", tokenTree.ToFullString());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_UnterminatedBodyReportsMissingBrace()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => #dsl {
                custom content
            """);

        var tokenTree = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroTokenTreeSyntax>()
            .Single();

        Assert.True(tokenTree.CloseBraceToken.IsMissing);
        Assert.Contains(tree.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV1003");
    }

    [Fact]
    public void BangMacroExpression_ParsesDedicatedNodeAndPreservesRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => quote! {
                left + right
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<BangMacroExpressionSyntax>()
            .Single();

        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("quote", macroName);
        Assert.Equal(SyntaxKind.ExclamationToken, expression.ExclamationToken.Kind);
        Assert.True(expression.ArgumentList.OpenParenToken.IsMissing);
        Assert.Contains("left + right", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void BangMacroExpression_ParsesGenericNameArgumentsAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => repeat<int>!(3, Label: "item") {
                custom content
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<BangMacroExpressionSyntax>()
            .Single();

        Assert.IsType<GenericNameSyntax>(expression.Name);
        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Equal("Label", expression.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Contains("custom content", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void BangMacroExpression_ParsesArgumentStyleInvocationWithoutBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => twice!(21)
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<BangMacroExpressionSyntax>()
            .Single();

        Assert.Equal("twice", expression.Name.ToString());
        Assert.Single(expression.ArgumentList.Arguments);
        Assert.Null(expression.TokenTree);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void BangMacroExpression_ParsesContextualKeywordName()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => add!(20, Right: 22)
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<BangMacroExpressionSyntax>()
            .Single();

        Assert.Equal("add", expression.Name.ToString());
        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Null(expression.TokenTree);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void BangMacroExpression_ParsesQualifiedGenericNameAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => Raven.Macros.Compile<Func<int>>! {
                42
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<BangMacroExpressionSyntax>()
            .Single();

        Assert.IsType<QualifiedNameSyntax>(expression.Name);
        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("Raven.Macros.Compile", macroName);
        Assert.Contains("42", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void HashMacroExpression_ParsesQualifiedName()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => #Example.Macros.Answer()
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<HashMacroExpressionSyntax>()
            .Single();

        Assert.IsType<QualifiedNameSyntax>(expression.Name);
        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("Example.Macros.Answer", macroName);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PostfixExclamationWithoutMacroBody_RemainsPostfixOperator()
    {
        var tree = SyntaxTree.ParseText("""
            func Main(value: string?) -> string => value!.ToString()
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<BangMacroExpressionSyntax>());
        Assert.Contains(
            tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>(),
            static expression => expression.Kind == SyntaxKind.SuppressNullableWarningExpression);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void LineBreakAfterExclamation_DoesNotStartBangMacroExpression()
    {
        var tree = SyntaxTree.ParseText("""
            func Main(value: string?) -> string {
                value!
                { value }
            }
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<BangMacroExpressionSyntax>());
        Assert.Contains(
            tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>(),
            static expression => expression.Kind == SyntaxKind.SuppressNullableWarningExpression);
    }
}
