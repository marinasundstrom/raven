using Shouldly;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxTokenTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void PlainToken_WithNoTrivia()
    {
        var token = SyntaxFactory.ReturnKeyword;

        var span = token.Span;
        var fullSpan = token.FullSpan;

        var str = token.ToString();
        var fullStr = token.ToFullString();
    }

    [Fact]
    public void AddLeadingTriviaToToken()
    {
        var token = SyntaxFactory.ReturnKeyword;
        var newToken = token.WithLeadingTrivia(Whitespace(" "));

        var span = newToken.Span;
        var fullSpan = newToken.FullSpan;

        var str = newToken.ToString();
        var fullStr = newToken.ToFullString();
    }

    [Fact]
    public void AddTrailingTriviaToToken()
    {
        var token = SyntaxFactory.ReturnKeyword;
        var newToken = token
                .WithTrailingTrivia(Whitespace(" "));

        var span = newToken.Span;
        var fullSpan = newToken.FullSpan;

        var str = newToken.ToString();
        var fullStr = newToken.ToFullString();
    }

    [Fact]
    public void AddBothLeadingAndTrailingTriviaToToken()
    {
        var token = SyntaxFactory.ReturnKeyword;
        var newToken = token
            .WithLeadingTrivia(Whitespace(" "))
            .WithTrailingTrivia(Newline());

        var span = newToken.Span;
        var fullSpan = newToken.FullSpan;

        var str = newToken.ToString();
        var fullStr = newToken.ToFullString();
    }
}