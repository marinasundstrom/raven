using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserRecoveryTests
{
    [Fact]
    public void StatementBlock_MissingCloseBrace_ProducesMissingToken()
    {
        var lexer = new Lexer(new StringReader("{ let x = 1"));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var block = Assert.IsType<BlockStatementSyntax>(parser.ParseStatement().CreateRed());

        Assert.True(block.CloseBraceToken.IsMissing);
        Assert.True(block.Statements.Count > 0);
    }

    [Fact]
    public void ExpressionBlock_MissingCloseBrace_ProducesMissingToken()
    {
        var lexer = new Lexer(new StringReader("{ 1"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var block = (BlockSyntax)parser.ParseBlockSyntax().CreateRed();

        Assert.True(block.CloseBraceToken.IsMissing);
        Assert.Single(block.Statements);
    }

    [Fact]
    public void InvocationArgumentList_TrailingCommaBeforeCloseParen_Recovers()
    {
        var lexer = new Lexer(new StringReader("Foo(1, )"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        var invocation = Assert.IsType<InvocationExpressionSyntax>(expression);

        Assert.False(invocation.ArgumentList.CloseParenToken.IsMissing);
        Assert.Single(invocation.ArgumentList.Arguments);
        Assert.Contains(parser.Diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidExpressionTerm);
    }

    [Fact]
    public void BracketedArgumentList_TrailingCommaBeforeCloseBracket_Recovers()
    {
        var lexer = new Lexer(new StringReader("foo[1, ]"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        var elementAccess = Assert.IsType<ElementAccessExpressionSyntax>(expression);

        Assert.False(elementAccess.ArgumentList.CloseBracketToken.IsMissing);
        Assert.Single(elementAccess.ArgumentList.Arguments);
        Assert.Contains(parser.Diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidExpressionTerm);
    }

    [Fact]
    public void CollectionExpression_MissingCloseBracket_ProducesMissingToken()
    {
        var lexer = new Lexer(new StringReader("[1,"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        var collection = Assert.IsType<CollectionExpressionSyntax>(expression);

        Assert.True(collection.CloseBracketToken.IsMissing);
        Assert.NotEmpty(collection.Elements);
    }
}
