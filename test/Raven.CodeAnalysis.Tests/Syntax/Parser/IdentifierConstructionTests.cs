using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public class IdentifierConstructionTests
{
    [Fact]
    public void BacktickIdentifier_RejectsEmptyName()
    {
        Assert.Contains(SyntaxTree.ParseText("let `` = 1").GetDiagnostics(), d => d.Id == "RAV1001");
    }

    [Theory]
    [InlineData("`Calculate total price`", "Calculate total price")]
    [InlineData("`@name`", "@name")]
    [InlineData("@class", "class")]
    public void IdentifierFactory_PreservesEscapedValue(string text, string name)
    {
        var token = Raven.CodeAnalysis.Syntax.SyntaxFactory.Identifier(text);
        Assert.Equal(text, token.Text);
        Assert.Equal(name, token.ValueText);
        Assert.Equal(name, Raven.CodeAnalysis.Syntax.SyntaxFactory.IdentifierName(text).Identifier.ValueText);
    }

}
