using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ThrowExpressionParserTests
{
    [Fact]
    public void ThrowExpression_ParsesAsExpression()
    {
        var lexer = new Lexer(new StringReader("throw Exception(\"missing\")"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());
        var throwExpression = Assert.IsType<ThrowExpressionSyntax>(expression);

        Assert.Equal(SyntaxKind.ThrowKeyword, throwExpression.ThrowKeyword.Kind);
        Assert.IsType<InvocationExpressionSyntax>(throwExpression.Expression);
    }

    [Fact]
    public void NullCoalesce_WithThrowExpression_ParsesThrowOnRight()
    {
        var lexer = new Lexer(new StringReader("name ?? throw Exception(\"missing\")"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());
        var coalesce = Assert.IsType<NullCoalesceExpressionSyntax>(expression);
        Assert.IsType<ThrowExpressionSyntax>(coalesce.Right);
    }
}
