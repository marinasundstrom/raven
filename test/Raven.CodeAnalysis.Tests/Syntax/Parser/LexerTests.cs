using System.IO;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class LexerTests
{
    [Fact]
    public void DotPrefixedDecimal_IsParsedAsNumericLiteral()
    {
        var lexer = new Lexer(new StringReader(".12"));
        var token = lexer.ReadToken();

        Assert.Equal(SyntaxKind.NumericLiteralToken, token.Kind);
        Assert.Equal(".12", token.Text);
        var value = Assert.IsType<double>(token.Value);
        Assert.Equal(0.12d, value);
    }
}
