using Shouldly;

using Xunit;
using Xunit.Abstractions;

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
        var triviaList = TriviaList(Whitespace("  "), Newline());

        var newTriviaList = triviaList.Remove(SyntaxKind.WhitespaceTrivia);

        triviaList.Width.ShouldBe(3);
        newTriviaList.ShouldNotBeSameAs(triviaList);
        newTriviaList.Width.ShouldBe(1);
    }
}
