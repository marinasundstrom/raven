using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class UnionParserContractTests
{
    [Theory]
    [InlineData("union Token {}", 0)]
    [InlineData("union Token { case A }", 1)]
    [InlineData("union Token { case A, case B }", 2)]
    [InlineData("union Token { case A\ncase B }", 2)]
    [InlineData("union Token<T> { case A(value: T)\ncase B }", 2)]
    [InlineData("[A] public union struct Token { case A }", 1)]
    public void DeclarationPreservesSourceAndCases(string source, int count)
    {
        var parser = Parser(source);
        var result = (UnionDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal("Token", result.Identifier.ValueText);
        Assert.Equal(count, result.Members.Count);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("union Token(int | string)", false)]
    [InlineData("union Token(int, string)", true)]
    [InlineData("union Token(int | string);", false)]
    [InlineData("union Token(int | string) {}", false)]
    public void StructuralMembersPreserveSource(string source, bool diagnostics)
    {
        var parser = Parser(source);
        var result = (UnionDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.NotNull(result.MemberTypes);
        Assert.Equal(diagnostics, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void ParsingStopsAtNextDeclaration()
    {
        var parser = Parser("union Token { case A }; class Next {}");
        var result = (UnionDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(SyntaxKind.SemicolonToken, result.TerminatorToken.Kind);
        Assert.Equal(SyntaxKind.ClassKeyword, parser.PeekToken().Kind);
        Assert.Empty(parser.Diagnostics);
    }

    private static UnionDeclarationParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
