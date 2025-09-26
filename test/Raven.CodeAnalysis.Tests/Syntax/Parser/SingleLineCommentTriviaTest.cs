namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class SingleLineCommentTriviaTest
{
    [Fact]
    public void SingleLineCommentTrivia_IsLeadingTriviaOfToken()
    {
        var code = """
                    if (foo)  {
                        // Foo bar
                        return 0;
                    }
                    """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var returnStatement = root.DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .First();

        var trivia = returnStatement.ReturnKeyword.LeadingTrivia.FirstOrDefault(x => x.Kind == SyntaxKind.SingleLineCommentTrivia);

        trivia.ShouldNotBeNull();
    }

    [Fact]
    public void SingleLineCommentTrivia_IsLeadingTriviaOfEndOFileToken()
    {
        var code = """
                    // Foo bar
                    """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var trivia = root.EndOfFileToken.LeadingTrivia.FirstOrDefault(x => x.Kind == SyntaxKind.SingleLineCommentTrivia);

        trivia.ShouldNotBeNull();
    }

    [Fact]
    public void SingleLineCommentTrivia_BeforeEof_IsLeadingTriviaOfEndOfFileToken()
    {
        var code = "let x = 1; // Foo bar";

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var trivia = root.DescendantTrivia(descendIntoStructuredTrivia: true)
            .FirstOrDefault(x => x.Kind == SyntaxKind.SingleLineCommentTrivia);

        trivia.ShouldNotBeNull();
    }

    [Theory]
    [InlineData("let x = 1; // Привет мир", "// Привет мир")]
    [InlineData("let x = 1; // Café au lait", "// Café au lait")]
    [InlineData("let x = 1; // 😀 emoji", "// 😀 emoji")]
    public void SingleLineCommentTrivia_PreservesUnicodeContent(string code, string expectedComment)
    {
        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var trivia = root.DescendantTokens()
            .SelectMany(t => t.LeadingTrivia.Concat(t.TrailingTrivia))
            .FirstOrDefault(x => x.Kind == SyntaxKind.SingleLineCommentTrivia);

        trivia.ShouldNotBeNull();
        trivia!.Text.ShouldBe(expectedComment);
    }

    [Theory]
    [InlineData("\u2028")]
    [InlineData("\u2029")]
    [InlineData("\u0085")]
    public void SingleLineCommentTrivia_TerminatesAtUnicodeNewline(string newline)
    {
        var code = $"// raven{newline}let x = 1";

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var trivia = root.DescendantTokens()
            .SelectMany(t => t.LeadingTrivia.Concat(t.TrailingTrivia))
            .FirstOrDefault(x => x.Kind == SyntaxKind.SingleLineCommentTrivia);

        trivia.ShouldNotBeNull();
        trivia!.Text.ShouldBe("// raven");
    }
}
