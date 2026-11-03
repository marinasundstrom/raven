using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class DirectiveParserContractTests
{
    [Theory]
    [InlineData("import System.Collections.Generic.*;", false)]
    [InlineData("import System.Collections.Generic.List<>;", false)]
    [InlineData("import ;", true)]
    public void ImportPreservesSourceAndMissingNameDiagnostics(string source, bool missingName)
    {
        var parser = new ImportDirectiveSyntaxParser(Context(source));
        var result = parser.ParseImportDirective();
        Assert.Equal(source, result.CreateRed().ToFullString());
        Assert.Equal(missingName ? 1 : 0, parser.Diagnostics.Count);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
        Assert.False(parser.TreatNewlinesAsTokens);
    }

    [Theory]
    [InlineData("alias Items = System.Collections.Generic.List<int>;", false)]
    [InlineData("alias Missing = ;", true)]
    public void AliasPreservesSourceAndMissingTypeDiagnostics(string source, bool missingType)
    {
        var parser = new AliasDirectiveSyntaxParser(Context(source));
        var result = parser.ParseAliasDirective();
        Assert.Equal(source, result.CreateRed().ToFullString());
        Assert.Equal(missingType ? 1 : 0, parser.Diagnostics.Count);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
        Assert.False(parser.TreatNewlinesAsTokens);
    }

    [Fact]
    public void NewlineSeparatesFollowingDirective()
    {
        var context = Context("import System\nimport System.Text");
        var first = new ImportDirectiveSyntaxParser(context).ParseImportDirective();
        Assert.Equal(SyntaxKind.ImportKeyword, context.PeekToken().Kind);
        var second = new ImportDirectiveSyntaxParser(context).ParseImportDirective();
        Assert.Equal("import System\nimport System.Text", first.CreateRed().ToFullString() + second.CreateRed().ToFullString());
        Assert.Empty(context.Diagnostics);
    }

    private static BaseParseContext Context(string source) => new(new Lexer(new StringReader(source)));
}
