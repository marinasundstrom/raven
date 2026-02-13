using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ReturnExpressionParserTests
{
    [Fact]
    public void ReturnExpression_ParsesAsExpression()
    {
        var lexer = new Lexer(new StringReader("return -1"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());
        var returnExpression = Assert.IsType<ReturnExpressionSyntax>(expression);

        Assert.Equal(SyntaxKind.ReturnKeyword, returnExpression.ReturnKeyword.Kind);
        Assert.IsType<LiteralExpressionSyntax>(returnExpression.Expression);
    }

    [Fact]
    public void NullCoalesce_WithReturnExpression_ParsesReturnOnRight()
    {
        var lexer = new Lexer(new StringReader("name ?? return -1"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());
        var coalesce = Assert.IsType<NullCoalesceExpressionSyntax>(expression);
        Assert.IsType<ReturnExpressionSyntax>(coalesce.Right);
    }
}
