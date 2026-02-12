using System;

using Raven.CodeAnalysis.Text;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class SyntaxFactoryEntryPointTests
{
    [Fact]
    public void ParseExpression_ParsesBinaryExpression()
    {
        var expression = SyntaxFactory.ParseExpression("1 + 2");

        Assert.IsType<BinaryExpressionSyntax>(expression);
    }

    [Fact]
    public void ParseExpression_ParsesBitwiseXorExpression()
    {
        var expression = SyntaxFactory.ParseExpression("1 ^ 2");

        var binary = Assert.IsType<BinaryExpressionSyntax>(expression);
        Assert.Equal(SyntaxKind.BitwiseXorExpression, binary.Kind);
    }

    [Fact]
    public void ParseExpression_ParsesBitwiseNotExpression()
    {
        var expression = SyntaxFactory.ParseExpression("~1");

        var unary = Assert.IsType<UnaryExpressionSyntax>(expression);
        Assert.Equal(SyntaxKind.BitwiseNotExpression, unary.Kind);
    }

    [Fact]
    public void ParseExpression_ParsesBlockExpression()
    {
        var expression = SyntaxFactory.ParseExpression("{ return 1; }");

        Assert.IsType<BlockSyntax>(expression);
    }

    [Fact]
    public void ParseStatement_ParsesReturnStatement()
    {
        var statement = SyntaxFactory.ParseStatement("return 1;");

        Assert.IsType<ReturnStatementSyntax>(statement);
    }

    [Fact]
    public void ParseStatement_WithPosition_ParsesFromOffset()
    {
        const string source = "val x = 0;\nreturn x;";
        var offset = source.IndexOf("return", StringComparison.Ordinal);

        var statement = SyntaxFactory.ParseStatement(SourceText.From(source), position: offset);

        Assert.IsType<ReturnStatementSyntax>(statement);
    }
}
