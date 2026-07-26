using Raven.CodeAnalysis.Syntax;

using Xunit;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class RavenQuoterTests
{
    [Fact]
    public void Quote_DefaultsToRavenOutput()
    {
        var quoted = RavenQuoter.QuoteText("let answer = 42");

        Assert.StartsWith("import Raven.CodeAnalysis.Syntax.*", quoted);
        Assert.Contains("import Raven.CodeAnalysis.Syntax.SyntaxFactory.*", quoted);
        Assert.DoesNotContain("using ", quoted);
        Assert.EndsWith(".NormalizeWhitespace()\n", quoted);
        Assert.Empty(SyntaxTree.ParseText(quoted).GetDiagnostics());
    }

    [Fact]
    public void Quote_CanGenerateCSharpOutput()
    {
        var quoted = RavenQuoter.QuoteText("let answer = 42", new RavenQuoterOptions
        {
            OutputLanguage = RavenQuoterOutputLanguage.CSharp
        });

        Assert.StartsWith("using Raven.CodeAnalysis.Syntax;", quoted);
        Assert.Contains("using static Raven.CodeAnalysis.Syntax.SyntaxFactory;", quoted);
        Assert.EndsWith(".NormalizeWhitespace();\n", quoted);
    }

    [Fact]
    public void Quote_RavenOutputUsesCollectionExpressions()
    {
        var quoted = RavenQuoter.QuoteText("""
            import System.*
            import System.Console.*
            """);

        Assert.Contains("[", quoted);
        Assert.Contains("]", quoted);
        Assert.DoesNotContain("new[]", quoted);
    }

    [Fact]
    public void Quote_RavenWrappedOutputParses()
    {
        var quoted = RavenQuoter.QuoteText("let answer = 42", new RavenQuoterOptions
        {
            WrapInClass = true
        });

        Assert.Contains("static class QuotedSyntax", quoted);
        Assert.Contains("static func Create() -> CompilationUnitSyntax", quoted);
        Assert.Empty(SyntaxTree.ParseText(quoted).GetDiagnostics());
    }

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
