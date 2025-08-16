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
}