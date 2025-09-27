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
    }
}


public class StructuredSyntaxTriviaTest
{
    [Fact]
    public void StructuredTrivia()
    {
        var x = IdentifierName(TrueKeyword)
            .WithTrailingTrivia(
                Trivia(
                    SkippedTokensTrivia(
                        TokenList(SemicolonToken))));

        var z = x.GetTrailingTrivia().First();
        var y = z.GetStructure();
    }

    [Fact]
    public void StructuredTrivia2()
    {
        var x = SemicolonToken
            .WithTrailingTrivia(
                Trivia(
                    SkippedTokensTrivia(
                        TokenList(IdentifierToken("Foo")))));

        var z = x.TrailingTrivia.First();
        var y = z.GetStructure();

        var returnStatement = ReturnStatement(ReturnKeyword, null, x);

        //z = returnStatement.TerminatorToken.Value.TrailingTrivia.First();
        //var o = z.GetStructure();
    }
}