namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxTriviaTest
{
    [Fact]
    public void CreateTriviaList_Empty()
    {
        var triviaList = TriviaList();

        triviaList.Width.ShouldBe(0);
    }

    [Fact]
    public void CreateTriviaList_WithOneTrivia()
    {
        var triviaList = TriviaList(Whitespace("  "));

        triviaList.Width.ShouldBe(2);
    }

    [Fact]
    public void AddTriviaToList()
    {
        var triviaList = TriviaList();

        var newTriviaList = triviaList.Add(Whitespace("  "));

        newTriviaList.ShouldNotBeSameAs(triviaList);
        newTriviaList.Width.ShouldBe(2);
    }

    [Fact]
    public void RemoveTriviaFromList_BySyntaxKind()
    {
        var triviaList = TriviaList(Whitespace("  "));

        var newTriviaList = triviaList.Remove(SyntaxKind.WhitespaceTrivia);

        triviaList.Width.ShouldBe(2);
        newTriviaList.ShouldNotBeSameAs(triviaList);
        newTriviaList.Width.ShouldBe(0);
    }

    [Fact]
    public void RemoveTriviaFromList_BySyntaxKind_OneShouldRemain()
    {
        var triviaList = TriviaList(Whitespace("  "), LineFeed);

        var newTriviaList = triviaList.Remove(SyntaxKind.WhitespaceTrivia);

        triviaList.Width.ShouldBe(3);
        newTriviaList.ShouldNotBeSameAs(triviaList);
        newTriviaList.Width.ShouldBe(1);
        newTriviaList.Count.ShouldBe(1);
        newTriviaList[0].Kind.ShouldBe(SyntaxKind.LineFeedTrivia);
    }

    [Fact]
    public void RemoveTriviaFromList_ByTrivia_RemovesMatchingEntries()
    {
        var trivia = Whitespace(" ");
        var triviaList = TriviaList(trivia, LineFeed, trivia);

        var updated = triviaList.Remove(trivia);

        updated.Count.ShouldBe(1);
        updated[0].Kind.ShouldBe(SyntaxKind.LineFeedTrivia);
        updated.Width.ShouldBe(1);
    }

    [Fact]
    public void TokenAttachedTrivia_HasCorrectPositions()
    {
        var token = ReturnKeyword
            .WithLeadingTrivia(Whitespace("  "))
            .WithTrailingTrivia(LineFeed);

        var leading = token.LeadingTrivia.Single();
        var trailing = token.TrailingTrivia.Single();

        leading.SpanStart.ShouldBe(0);
        leading.Span.Length.ShouldBe(2);
        trailing.SpanStart.ShouldBe(6);
        trailing.Span.Length.ShouldBe(1);
    }
}


public class StructuredSyntaxTriviaTest
{
    [Fact]
    public void StructuredTrivia()
    {
        var node = IdentifierName(TrueKeyword)
            .WithTrailingTrivia(
                Trivia(
                    SkippedTokensTrivia(
                        TokenList(SemicolonToken))));

        var trivia = node.GetTrailingTrivia().Single();
        var structure = trivia.GetStructure();

        trivia.HasStructure.ShouldBeTrue();
        structure.ShouldNotBeNull();
        structure.ShouldBeOfType<SkippedTokensTrivia>();

        var skipped = (SkippedTokensTrivia)structure;
        skipped.Tokens.Count.ShouldBe(1);
        skipped.Tokens[0].Kind.ShouldBe(SyntaxKind.SemicolonToken);
        trivia.ToString().ShouldBe(";");
    }

    [Fact]
    public void StructuredTrivia2()
    {
        var trivia = Trivia(
            SkippedTokensTrivia(
                TokenList(Identifier("Foo"))));
        var structure = trivia.GetStructure();

        trivia.HasStructure.ShouldBeTrue();
        structure.ShouldNotBeNull();
        structure.ShouldBeOfType<SkippedTokensTrivia>();
        structure.ToString().ShouldBe("Foo");

        var skipped = (SkippedTokensTrivia)structure;
        skipped.Tokens.Count.ShouldBe(1);
        skipped.Tokens[0].Kind.ShouldBe(SyntaxKind.IdentifierToken);
        skipped.Tokens[0].Text.ShouldBe("Foo");
    }
}
