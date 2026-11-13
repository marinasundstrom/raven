using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

[Trait("Target", "Shared")]
public sealed class MacroSyntaxTests
{
    [Fact]
    public void MacroSyntax_ProvidesStructuralAndFactoryForms()
    {
        var syntax = SyntaxFactory.ParseExpression("left + right")!;

        var structure = MacroSyntax.GetStructure(syntax);
        var factory = MacroSyntax.GetFactoryForm(syntax);

        Assert.Contains("AddExpression", structure);
        Assert.Contains("SyntaxFactory", factory);
        Assert.Contains("IdentifierName", factory);
    }

    [Theory]
    [InlineData("")]
    [InlineData("plain text")]
    [InlineData("${value}")]
    [InlineData("$value")]
    [InlineData("quote\" and slash\\")]
    [InlineData("\0\a\b\f\n\r\t\v")]
    [InlineData("\u0001\u001f\u007f\u0085")]
    [InlineData("å日本語😀")]
    public void StringLiteral_PreservesValueAndRoundTrips(string value)
    {
        var literal = MacroSyntax.StringLiteral(value);
        Assert.Equal(value, literal.Token.Value);
        var reparsed = Assert.IsType<LiteralExpressionSyntax>(SyntaxFactory.ParseExpression(literal.ToString()));
        Assert.Equal(value, reparsed.Token.Value);
        Assert.DoesNotContain(reparsed.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Helpers_RejectNull()
    {
        Assert.Throws<ArgumentNullException>(() => MacroSyntax.StringLiteral(null!));
        Assert.Throws<ArgumentNullException>(() => MacroSyntax.GetStructure(null!));
        Assert.Throws<ArgumentNullException>(() => MacroSyntax.GetFactoryForm(null!));
    }
}
