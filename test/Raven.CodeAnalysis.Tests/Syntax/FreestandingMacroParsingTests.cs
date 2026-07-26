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
            .OfType<FreestandingMacroExpressionSyntax>()
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
}
