namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class MultiLineCommentTriviaTest
{
    [Fact]
    public void MultiLineCommentTrivia_IsLeadingTriviaOfToken()
    {
        var code = """
                    if (foo)  {
                        /* Foo bar
                        return 0; */
                    }
                    """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var ifExpression = root.DescendantNodes()
            .OfType<IfExpressionSyntax>()
            .First();

        var trivia = ((BlockSyntax)ifExpression.Expression).CloseBraceToken.LeadingTrivia.FirstOrDefault(x => x.Kind == SyntaxKind.MultiLineCommentTrivia);

        trivia.ShouldNotBeNull();
    }

    [Fact(Skip = "Fix")]
    public void MultiLineCommentTrivia_IsLeadingTriviaOfEndOFileToken()
    {
        var code = """
                    /* Foo bar 
                    ff */
                    """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var trivia = root.EndOfFileToken.LeadingTrivia.FirstOrDefault(x => x.Kind == SyntaxKind.MultiLineCommentTrivia);

        trivia.ShouldNotBeNull();
    }
}