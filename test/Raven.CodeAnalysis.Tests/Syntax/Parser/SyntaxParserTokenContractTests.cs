using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class SyntaxParserTokenContractTests
{
    [Fact]
    public void MissingAndOptionalTokensDoNotConsumeLookahead()
    {
        var parser = Create("value");
        Assert.True(parser.IsNextToken(SyntaxKind.IdentifierToken));
        Assert.False(parser.ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var missing));
        Assert.True(missing.IsMissing);
        Assert.Equal(SyntaxKind.SemicolonToken, missing.Kind);
        Assert.False(parser.ConsumeTokenOrNone(SyntaxKind.SemicolonToken, out var none));
        Assert.Equal(SyntaxKind.None, none.Kind);
        Assert.False(parser.ConsumeTokenOrNull(SyntaxKind.SemicolonToken, out var absent));
        Assert.Null(absent);
        Assert.Equal(0, parser.Position);
        Assert.True(parser.ConsumeToken(SyntaxKind.IdentifierToken, out var token));
        Assert.Equal("value", token.Text);
        Assert.Equal(5, parser.Position);
    }

    [Theory]
    [InlineData("\nnext", SyntaxKind.None, SyntaxKind.IdentifierToken)]
    [InlineData(";next", SyntaxKind.SemicolonToken, SyntaxKind.IdentifierToken)]
    [InlineData("", SyntaxKind.None, SyntaxKind.EndOfFileToken)]
    [InlineData("}", SyntaxKind.None, SyntaxKind.CloseBraceToken)]
    [InlineData("let next = 1", SyntaxKind.None, SyntaxKind.LetKeyword)]
    public void TerminatorRecognizesExplicitAndImplicitBoundaries(string source, SyntaxKind expected, SyntaxKind next)
    {
        var parser = Create(source);
        Assert.True(parser.TryConsumeTerminator(out var terminator));
        Assert.Equal(expected, terminator.Kind);
        Assert.Equal(next, parser.PeekToken().Kind);
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void SplitLookaheadPreservesTextTriviaAndReadPosition()
    {
        var parser = Create("  >> // tail");
        Assert.True(parser.Split());
        Assert.Equal(0, parser.Position);
        var first = parser.ReadToken();
        var second = parser.ReadToken();
        Assert.Equal(SyntaxKind.GreaterThanToken, first.Kind);
        Assert.Equal(SyntaxKind.GreaterThanToken, second.Kind);
        Assert.Equal(">", first.Text);
        Assert.Equal(">", second.Text);
        Assert.Equal(2, first.LeadingTrivia.FullWidth);
        Assert.Equal(0, first.TrailingTrivia.FullWidth);
        Assert.Equal(0, second.LeadingTrivia.FullWidth);
        Assert.True(second.TrailingTrivia.FullWidth > 0);
        Assert.Equal(first.FullWidth + second.FullWidth, parser.Position);
    }

    [Fact]
    public void ActualSpanExcludesTriviaAndHandlesAbsentNode()
    {
        var parser = Create("  value  ");
        var token = parser.ReadToken();
        Assert.Equal(new TextSpan(2, 5), parser.ActualSpan(0, new InternalSyntax.IdentifierNameSyntax(token)));
        Assert.Equal(new TextSpan(0, 0), parser.ActualSpan(-5, null));
    }

    [Fact]
    public void MisplacedDirectiveRetainsTokensAndTrivia()
    {
        const string source = "import System.* // comment\n";
        var directive = new InternalSyntax.Parser.ImportDirectiveSyntaxParser(
            new InternalSyntax.Parser.BaseParseContext(new Lexer(new StringReader(source)))).ParseImportDirective();
        var recovered = Probe.Preserve(directive).CreateRed();
        Assert.Equal(source, recovered.ToFullString());
        Assert.Equal(SyntaxKind.IncompleteMemberDeclaration, recovered.Kind);
    }

    private static Probe Create(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));

    private sealed class Probe(ParseContext context) : SyntaxParser(context)
    {
        public static InternalSyntax.IncompleteMemberDeclarationSyntax Preserve(InternalSyntax.SyntaxNode node) => PreserveMisplacedDirective(node);

        public bool Split() => TrySplitLeadingLookaheadToken(SyntaxKind.GreaterThanGreaterThanToken,
            SyntaxKind.GreaterThanToken, ">", SyntaxKind.GreaterThanToken, ">");

        public TextSpan ActualSpan(int start, InternalSyntax.SyntaxNode? node) => GetActualTextSpan(start, node);
    }
}
