using Shouldly;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxTokenTest
{
    [Fact]
    public void MissingToken()
    {
        var token = SyntaxFactory.MissingToken(SyntaxKind.BoolKeyword);

        token.Kind.ShouldBe(SyntaxKind.BoolKeyword);

        token.Width.ShouldBe(0);
        token.FullWidth.ShouldBe(0);

        token.Span.Start.ShouldBe(0);
        token.Span.Length.ShouldBe(0);
        token.Span.End.ShouldBe(0);

        token.ToString().ShouldBe(string.Empty);
        token.ToFullString().ShouldBe(string.Empty);
    }

    [Fact]
    public void IdentifierToken()
    {
        var token = SyntaxFactory.IdentifierToken("test");

        token.Kind.ShouldBe(SyntaxKind.IdentifierToken);

        token.Width.ShouldBe(4);
        token.FullWidth.ShouldBe(4);

        token.Span.Start.ShouldBe(0);
        token.Span.Length.ShouldBe(4);
        token.Span.End.ShouldBe(4);

        token.ToString().ShouldBe("test");
        token.ToFullString().ShouldBe("test");
    }

    [Fact]
    public void Keyword()
    {
        var token = SyntaxFactory.ReturnKeyword;

        token.Kind.ShouldBe(SyntaxKind.ReturnKeyword);

        token.Width.ShouldBe(6);
        token.FullWidth.ShouldBe(6);

        token.Span.Start.ShouldBe(0);
        token.Span.Length.ShouldBe(6);
        token.Span.End.ShouldBe(6);

        token.ToString().ShouldBe("return");
        token.ToFullString().ShouldBe("return");
    }

    [Fact]
    public void AddLeadingTriviaToToken()
    {
        var originalToken = SyntaxFactory.ReturnKeyword;
        var token = originalToken.WithLeadingTrivia(Whitespace(" "));

        token.ShouldNotBeSameAs(originalToken);

        token.Width.ShouldBe(6);
        token.FullWidth.ShouldBe(7);

        token.Span.Start.ShouldBe(1);
        token.Span.Length.ShouldBe(6);
        token.Span.End.ShouldBe(7);

        token.FullSpan.Start.ShouldBe(0);
        token.FullSpan.Length.ShouldBe(7);
        token.Span.End.ShouldBe(7);

        token.ToString().ShouldBe("return");
        token.ToFullString().ShouldBe(" return");
    }

    [Fact]
    public void AddTrailingTriviaToToken()
    {
        var originalToken = SyntaxFactory.ReturnKeyword;
        var token = originalToken.WithTrailingTrivia(Whitespace(" "));

        token.ShouldNotBeSameAs(originalToken);

        token.Width.ShouldBe(6);
        token.FullWidth.ShouldBe(7);

        token.Span.Start.ShouldBe(0);
        token.Span.Length.ShouldBe(6);
        token.Span.End.ShouldBe(6);

        token.FullSpan.Start.ShouldBe(0);
        token.FullSpan.Length.ShouldBe(7);
        token.FullSpan.End.ShouldBe(7);

        token.ToString().ShouldBe("return");
        token.ToFullString().ShouldBe("return ");
    }

    [Fact]
    public void AddBothLeadingAndTrailingTriviaToToken()
    {
        var token = SyntaxFactory.ReturnKeyword
            .WithLeadingTrivia(Whitespace(" "))
            .WithTrailingTrivia(LineFeed);

        token.Width.ShouldBe(6);
        token.FullWidth.ShouldBe(8);

        token.Span.Start.ShouldBe(1);
        token.Span.Length.ShouldBe(6);
        token.Span.End.ShouldBe(7);

        token.FullSpan.Start.ShouldBe(0);
        token.FullSpan.Length.ShouldBe(8);
        token.FullSpan.End.ShouldBe(8);

        token.ToString().ShouldBe("return");
        token.ToFullString().ShouldBe(" return\n");
    }
}