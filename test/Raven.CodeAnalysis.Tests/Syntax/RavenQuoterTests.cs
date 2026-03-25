using Raven.CodeAnalysis.Syntax;

using Xunit;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class RavenQuoterTests
{
    [Fact]
    public void Quote_UsesSmallestMatchingFactoryOverload()
    {
        var node = ParenthesizedExpression(IdentifierName("value"));

        var quoted = RavenQuoter.Quote(node, new RavenQuoterOptions
        {
            GenerateUsingDirectives = false,
            UseNamedArguments = true
        });

        Assert.Contains("ParenthesizedExpression(", quoted);
        Assert.Contains("expression:", quoted);
        Assert.DoesNotContain("openParenToken:", quoted);
        Assert.DoesNotContain("closeParenToken:", quoted);
    }

    [Fact]
    public void Quote_UsesLargerFactoryOverloadWhenDefaultTokensDoNotMatch()
    {
        var node = ParenthesizedExpression(
            OpenParenToken.WithLeadingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " "))),
            IdentifierName("value"),
            CloseParenToken);

        var quoted = RavenQuoter.Quote(node, new RavenQuoterOptions
        {
            GenerateUsingDirectives = false,
            IncludeTrivia = true,
            UseNamedArguments = true
        });

        Assert.Contains("openParenToken:", quoted);
        Assert.Contains("closeParenToken:", quoted);
    }

    [Fact]
    public void Quote_PicksMatchingSameArityFactoryOverload()
    {
        var node = ParameterlessConstructorDeclaration(
            List<AttributeListSyntax>(),
            TokenList(),
            ArrowExpressionClause(IdentifierName("value")));

        var quoted = RavenQuoter.Quote(node, new RavenQuoterOptions
        {
            GenerateUsingDirectives = false,
            UseNamedArguments = true
        });

        Assert.Contains("ParameterlessConstructorDeclaration(", quoted);
        Assert.Contains("expressionBody:", quoted);
        Assert.DoesNotContain("body:", quoted);
    }

    [Fact]
    public void Quote_OmitsOptionalTerminatorTokenWhenDefaultNoneMatches()
    {
        var node = ExpressionStatement(IdentifierName("value"));

        var quoted = RavenQuoter.Quote(node, new RavenQuoterOptions
        {
            GenerateUsingDirectives = false,
            UseNamedArguments = true
        });

        Assert.Contains("ExpressionStatement(", quoted);
        Assert.Contains("expression:", quoted);
        Assert.DoesNotContain("terminatorToken:", quoted);
    }
}
