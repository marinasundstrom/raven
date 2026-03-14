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
}
