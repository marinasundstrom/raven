using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class AwaitExpressionTests
{
    [Fact]
    public void AwaitExpression_ParsesAsUnaryExpression()
    {
        var lexer = new Lexer(new StringReader("await foo"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());
        var awaitExpression = Assert.IsType<UnaryExpressionSyntax>(expression);

        Assert.Equal(SyntaxKind.AwaitExpression, awaitExpression.Kind);
        Assert.Equal(SyntaxKind.AwaitKeyword, awaitExpression.OperatorToken.Kind);

        var operand = Assert.IsType<IdentifierNameSyntax>(awaitExpression.Expression);
        Assert.Equal("foo", operand.Identifier.ValueText);
    }
}
