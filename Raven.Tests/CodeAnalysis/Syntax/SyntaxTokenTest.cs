using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxTokenTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test1()
    {
        var token = SyntaxFactory.ReturnKeyword;
        var newToken = token.WithLeadingTrivia(Whitespace(" "));

        var span = newToken.Span;
        var fullSpan = newToken.FullSpan;
    }

    [Fact]
    public void Test2()
    {
        var token = SyntaxFactory.ReturnKeyword;
        var newToken = token
                .WithTrailingTrivia(Whitespace(" "));

        var span = newToken.Span;
        var fullSpan = newToken.FullSpan;
    }
    
    [Fact]
    public void Test3()
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