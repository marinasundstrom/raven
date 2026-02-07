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
        const string source = "let x = 0;\nreturn x;";
        var offset = source.IndexOf("return", StringComparison.Ordinal);

        var statement = SyntaxFactory.ParseStatement(SourceText.From(source), position: offset);

        Assert.IsType<ReturnStatementSyntax>(statement);
    }
}
