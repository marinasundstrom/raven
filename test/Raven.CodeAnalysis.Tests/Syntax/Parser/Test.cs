using Xunit;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserNewlineTests
{
    [Fact]
    public void Statement_NewlineActsAsTerminator_WhenOutsideParens()
    {
        // Arrange
        var source = "let x = 42\nlet y = 21\n";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        // Act
        var s1 = parser.ParseStatement();
        var s2 = parser.ParseStatement();


        var firstStatement = s1.CreateRed();
        var secondStatement = s2.CreateRed();

        // Assert
        Assert.NotNull(firstStatement);
        Assert.NotNull(secondStatement);

        var firstToken = firstStatement.GetLastToken();
        Assert.Equal(SyntaxKind.NewLineToken, firstToken.Kind);
        Assert.Equal(SyntaxKind.NewLineToken, context.LastToken?.Kind); // newline was consumed as terminator
    }

    [Fact]
    public void Statement_SemicolonActsAsTerminator_WhenOutsideParens()
    {
        // Arrange
        var source = "let x = 42;let y = 21\n";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        // Act
        var s1 = parser.ParseStatement();
        var s2 = parser.ParseStatement();


        var firstStatement = s1.CreateRed();
        var secondStatement = s2.CreateRed();

        // Assert
        Assert.NotNull(firstStatement);
        Assert.NotNull(secondStatement);

        var firstToken = firstStatement.GetLastToken();
        Assert.Equal(SyntaxKind.SemicolonToken, firstToken.Kind);
        Assert.Equal(SyntaxKind.NewLineToken, context.LastToken?.Kind); // semicolon was consumed as terminator
    }

    [Fact]
    public void Statement_NewlineIsTrivia_WhenInLineContinuation()
    {
        // Arrange
        var source = "let x =\n    42\n";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        // Act
        var s = parser.ParseStatement();

        var statement = s.CreateRed();

        // Assert
        var literalToken = statement.DescendantTokens().FirstOrDefault(t => t.Kind == SyntaxKind.NumericLiteralToken);
        Assert.NotNull(literalToken);

        var newlineTrivia = literalToken.LeadingTrivia.FirstOrDefault(t => t.Kind == SyntaxKind.EndOfLineTrivia);
        Assert.NotNull(newlineTrivia); // newline was treated as trivia
    }

    [Fact]
    public void Statement_SemicolonActsAsTerminator()
    {
        // Arrange
        var source = "let x = 42;";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        // Act
        var s = parser.ParseStatement();

        var statement = s.CreateRed();

        // Assert
        var lastToken = statement.GetLastToken();
        Assert.Equal(SyntaxKind.SemicolonToken, context.LastToken?.Kind);
    }

    [Theory]
    [InlineData("let x = 1;", SyntaxKind.SemicolonToken)]
    [InlineData("let x = 1\n", SyntaxKind.NewLineToken)]
    [InlineData("let x = 1", SyntaxKind.EndOfFileToken)]
    public void Statement_Terminators_AreRecognizedCorrectly(string source, SyntaxKind expectedKind)
    {
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new SyntaxParser(context);

        // Read until just before the terminator
        var let = parser.ExpectToken(SyntaxKind.LetKeyword);
        var id = parser.ExpectToken(SyntaxKind.IdentifierToken);
        var eq = parser.ExpectToken(SyntaxKind.EqualsToken);
        var lit = parser.ExpectToken(SyntaxKind.NumericLiteralToken);

        Assert.True(parser.TryConsumeTerminator(out var terminator));
        Assert.Equal(expectedKind, terminator.Kind);
    }
}
