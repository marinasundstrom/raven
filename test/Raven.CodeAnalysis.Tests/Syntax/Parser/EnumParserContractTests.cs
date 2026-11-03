using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class EnumParserContractTests
{
    [Theory]
    [InlineData("enum Colors {}", 0, false)]
    [InlineData("enum Colors { Red, Green, Blue }", 3, false)]
    [InlineData("enum Colors { Red; Green; Blue; }", 3, false)]
    [InlineData("enum Colors { Red\nGreen\nBlue }", 3, false)]
    [InlineData("[Flags] public enum Colors : byte { [A] Red = 1, Green = 2 }", 2, false)]
    [InlineData("enum Colors { Red Green }", 2, true)]
    [InlineData("enum Colors { Red, Green; Blue }", 3, true)]
    [InlineData("enum Colors { Red, Green\nBlue }", 3, true)]
    public void DeclarationPreservesSourceMembersAndSeparatorDiagnostics(string source, int count, bool diagnostics)
    {
        var parser = Parser(source);
        var result = (EnumDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal("Colors", result.Identifier.ValueText);
        Assert.Equal(count, result.Members.Count);
        Assert.Equal(diagnostics, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData(",", SyntaxKind.CommaToken)]
    [InlineData(";", SyntaxKind.SemicolonToken)]
    [InlineData("\n", SyntaxKind.None)]
    public void SeparatorKindsRemainExplicit(string separator, SyntaxKind kind)
    {
        var parser = Parser($"enum Colors {{ Red{separator}Green }}");
        var result = (EnumDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(kind, result.Members.GetSeparator(0).Kind);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("enum Colors { + }")]
    [InlineData("enum Colors { +, Red }")]
    [InlineData("enum Colors { [A] +, Red }")]
    public void UnexpectedMemberTokensRecoverAndPreserveSource(string source)
    {
        var parser = Parser(source);
        var result = (EnumDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.True(result.Members[0].Identifier.IsMissing);
        Assert.Contains(parser.Diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void ParsingStopsAfterDeclarationTerminator()
    {
        var parser = Parser("enum Colors { Red }; class Next {}");
        var result = (EnumDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(SyntaxKind.SemicolonToken, result.TerminatorToken.Kind);
        Assert.Equal(SyntaxKind.ClassKeyword, parser.PeekToken().Kind);
        Assert.Empty(parser.Diagnostics);
    }

    private static EnumDeclarationParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
