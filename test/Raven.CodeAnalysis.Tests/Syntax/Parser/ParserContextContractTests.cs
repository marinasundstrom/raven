using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserContextContractTests
{
    [Fact]
    public void SyntaxParser_UsesParseContextParenthesisState()
    {
        Assert.Equal(typeof(ParseContext), typeof(SyntaxParser).GetMethod(nameof(ParseContext.EnterParens))?.DeclaringType);
        Assert.Equal(typeof(ParseContext), typeof(SyntaxParser).GetMethod(nameof(ParseContext.ExitParens))?.DeclaringType);
        Assert.Equal(typeof(ParseContext), typeof(SyntaxParser).GetProperty(nameof(ParseContext.IsInsideParens))?.DeclaringType);
    }
}
