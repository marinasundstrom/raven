using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

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
        Assert.Equal(SyntaxKind.NewLineToken, secondStatement.GetLastToken(true).Kind);
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


        var firstStatement = (StatementSyntax)s1.CreateRed();
        var secondStatement = (StatementSyntax)s2.CreateRed();

        // Assert
        Assert.NotNull(firstStatement);
        Assert.NotNull(secondStatement);

        var firstToken = firstStatement.GetLastToken();
        Assert.Equal(SyntaxKind.SemicolonToken, firstToken.Kind);
        Assert.Equal(SyntaxKind.NewLineToken, secondStatement.GetLastToken(true).Kind);
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
        Assert.Equal(SyntaxKind.NumericLiteralToken, literalToken.Kind);

        var newlineTrivia = literalToken.LeadingTrivia.FirstOrDefault(t => t.Kind == SyntaxKind.EndOfLineTrivia);
        Assert.Equal(SyntaxKind.EndOfLineTrivia, newlineTrivia.Kind);
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
    [InlineData("let x = 1", SyntaxKind.None)]
    [InlineData("let x = 1}", SyntaxKind.None)]
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

    [Fact]
    public void Terminator_SkipsMisplacedTokens_BeforeNewline()
    {
        var source = "let x = 1 foo\n";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new SyntaxParser(context);

        parser.ExpectToken(SyntaxKind.LetKeyword);
        parser.ExpectToken(SyntaxKind.IdentifierToken);
        parser.ExpectToken(SyntaxKind.EqualsToken);
        parser.ExpectToken(SyntaxKind.NumericLiteralToken);

        Assert.True(parser.TryConsumeTerminator(out var terminator));
        Assert.Equal(SyntaxKind.NewLineToken, terminator.Kind);

        var redTerminator = (SyntaxToken)terminator;
        var skipped = redTerminator.LeadingTrivia.Single(t => t.Kind == SyntaxKind.SkippedTokensTrivia);
        var skippedNode = (SkippedTokensTrivia)skipped.GetStructure()!;
        Assert.Equal(SyntaxKind.IdentifierToken, skippedNode.Tokens.Single().Kind);
    }

    [Fact]
    public void Terminator_SkipsMisplacedTokens_BeforeSemicolon()
    {
        var source = "let x = 1 foo;";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new SyntaxParser(context);

        parser.ExpectToken(SyntaxKind.LetKeyword);
        parser.ExpectToken(SyntaxKind.IdentifierToken);
        parser.ExpectToken(SyntaxKind.EqualsToken);
        parser.ExpectToken(SyntaxKind.NumericLiteralToken);

        Assert.True(parser.TryConsumeTerminator(out var terminator));
        Assert.Equal(SyntaxKind.SemicolonToken, terminator.Kind);

        var redTerminator = (SyntaxToken)terminator;
        var skipped = redTerminator.LeadingTrivia.Single(t => t.Kind == SyntaxKind.SkippedTokensTrivia);
        var skippedNode = (SkippedTokensTrivia)skipped.GetStructure()!;
        Assert.Equal(SyntaxKind.IdentifierToken, skippedNode.Tokens.Single().Kind);
    }

    [Fact]
    public void Terminator_SkipsTokens_UntilEndOfFile()
    {
        var source = "let x = 1 foo";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new SyntaxParser(context);

        parser.ExpectToken(SyntaxKind.LetKeyword);
        parser.ExpectToken(SyntaxKind.IdentifierToken);
        parser.ExpectToken(SyntaxKind.EqualsToken);
        parser.ExpectToken(SyntaxKind.NumericLiteralToken);

        Assert.True(parser.TryConsumeTerminator(out var terminator));
        Assert.Equal(SyntaxKind.None, terminator.Kind);

        var eof = (SyntaxToken)parser.PeekToken();
        var skipped = eof.LeadingTrivia.Single(t => t.Kind == SyntaxKind.SkippedTokensTrivia);
        var skippedNode = (SkippedTokensTrivia)skipped.GetStructure()!;
        Assert.Equal(SyntaxKind.IdentifierToken, skippedNode.Tokens.Single().Kind);
    }

    [Fact]
    public void Block_LastStatementWithoutTerminator_UsesNoneToken()
    {
        var source = "{ return \"\" }";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var block = (BlockSyntax)parser.ParseBlockSyntax().CreateRed();

        var returnStatement = block.Statements.OfType<ReturnStatementSyntax>().Single();

        var terminator = returnStatement.TerminatorToken;
        Assert.Equal(SyntaxKind.None, terminator.Kind);
    }

    [Fact]
    public void ReturnStatement_BeforeElse_UsesNoneTerminator()
    {
        var source = "return else";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (ReturnStatementSyntax)parser.ParseStatement().CreateRed();
        Assert.Equal(SyntaxKind.None, statement.TerminatorToken.Kind);

        var nextToken = parser.PeekToken();
        Assert.Equal(SyntaxKind.ElseKeyword, nextToken.Kind);
    }

    [Fact]
    public void Function_MissingIdentifier_ProducesMissingToken()
    {
        var source = "func () {}";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (FunctionStatementSyntax)parser.ParseStatement().CreateRed();

        Assert.True(statement.Identifier.IsMissing);
    }

    [Fact]
    public void VariableDeclaration_MissingIdentifier_ProducesMissingToken()
    {
        var source = "let = 1";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (LocalDeclarationStatementSyntax)parser.ParseStatement().CreateRed();
        var declarator = statement.Declaration.Declarators.Single();

        Assert.True(declarator.Identifier.IsMissing);
    }

    [Theory]
    [InlineData("変数")]
    [InlineData("данные")]
    [InlineData("δοκιμή")]
    [InlineData("مجموع")]
    public void VariableDeclaration_AllowsUnicodeIdentifier(string identifier)
    {
        var source = $"let {identifier} = 1";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (LocalDeclarationStatementSyntax)parser.ParseStatement().CreateRed();
        var declarator = statement.Declaration.Declarators.Single();

        Assert.Equal(identifier, declarator.Identifier.Text);
        Assert.False(declarator.Identifier.IsMissing);
    }

    [Theory]
    [InlineData("世界")]
    [InlineData("данные")]
    [InlineData("δοκιμή")]
    [InlineData("مجموع")]
    public void VariableDeclaration_AllowsUnicodeStringLiteral(string literal)
    {
        var source = $"let text = \"{literal}\"";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (LocalDeclarationStatementSyntax)parser.ParseStatement().CreateRed();
        var declarator = statement.Declaration.Declarators.Single();

        var initializer = declarator.Initializer;
        Assert.NotNull(initializer);
        var literalExpression = Assert.IsType<LiteralExpressionSyntax>(initializer!.Value);

        Assert.Equal(SyntaxKind.StringLiteralExpression, literalExpression.Kind);
        Assert.Equal(literal, literalExpression.Token.ValueText);
        Assert.Equal(literal, Assert.IsType<string>(literalExpression.Token.Value));
    }

    [Fact]
    public void ParameterList_MissingIdentifier_ProducesMissingToken()
    {
        var source = "func foo(: int) {}";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (FunctionStatementSyntax)parser.ParseStatement().CreateRed();
        var parameter = statement.ParameterList.Parameters.Single();

        Assert.True(parameter.Identifier.IsMissing);
    }

    [Fact]
    public void Parameter_WithDefaultValue_ParsesEqualsValueClause()
    {
        var source = "func foo(bar: int = 42) {}";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (FunctionStatementSyntax)parser.ParseStatement().CreateRed();
        var parameter = statement.ParameterList.Parameters.Single();

        Assert.NotNull(parameter.DefaultValue);
        Assert.Equal(SyntaxKind.NumericLiteralExpression, parameter.DefaultValue!.Value.Kind);
    }

    [Fact]
    public void SkipUntil_AtEndOfFile_ReturnsNoneToken()
    {
        var source = string.Empty;
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);

        var token = context.SkipUntil(SyntaxKind.SemicolonToken, SyntaxKind.NewLineToken);

        Assert.Equal(SyntaxKind.None, token.Kind);
        Assert.Equal(SyntaxKind.EndOfFileToken, context.PeekToken().Kind);
    }

    [Fact]
    public void TryStatement_WithCatchAndFinally_ParsesClauses()
    {
        var source = "try { } catch (Exception ex) { } finally { }";
        var lexer = new Lexer(new StringReader(source));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var statement = (TryStatementSyntax)parser.ParseStatement().CreateRed();

        Assert.Equal(SyntaxKind.TryStatement, statement.Kind);
        Assert.Single(statement.CatchClauses);

        var catchClause = statement.CatchClauses[0];
        Assert.NotNull(catchClause.Declaration);
        Assert.Equal("ex", catchClause.Declaration!.Identifier?.Text);
        Assert.NotNull(statement.FinallyClause);
    }
}
