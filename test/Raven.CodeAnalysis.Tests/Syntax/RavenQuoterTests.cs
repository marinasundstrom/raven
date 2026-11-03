#if RAVEN_QUOTER_PORT
extern alias Port;
using QuoterUnderTest = Port::Raven.CodeAnalysis.Syntax.RavenQuoter;
using QuoterOptionsUnderTest = Port::Raven.CodeAnalysis.Syntax.RavenQuoterOptions;
using QuoterLanguageUnderTest = Port::Raven.CodeAnalysis.Syntax.RavenQuoterOutputLanguage;
#else
using QuoterUnderTest = Raven.CodeAnalysis.Syntax.RavenQuoter;
using QuoterOptionsUnderTest = Raven.CodeAnalysis.Syntax.RavenQuoterOptions;
using QuoterLanguageUnderTest = Raven.CodeAnalysis.Syntax.RavenQuoterOutputLanguage;
#endif
using Raven.CodeAnalysis.Syntax;

using Xunit;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class RavenQuoterTests
{
    [Fact]
    public void Quote_PreservesNamedArgumentContract()
    {
        var root = ParseCompilationUnit("let answer = 42");
        Assert.Equal(QuoterUnderTest.Quote(root), QuoterUnderTest.Quote(root: root, options: null));
        Assert.Equal(QuoterUnderTest.QuoteText("let answer = 42"),
            QuoterUnderTest.QuoteText(ravenSource: "let answer = 42", options: null));
    }

    [Fact]
    public void Quote_DefaultsToRavenOutput()
    {
        var quoted = QuoterUnderTest.QuoteText("let answer = 42");

        Assert.StartsWith("import Raven.CodeAnalysis.Syntax.*", quoted);
        Assert.Contains("import Raven.CodeAnalysis.Syntax.SyntaxFactory.*", quoted);
        Assert.DoesNotContain("using ", quoted);
        Assert.EndsWith(".NormalizeWhitespace()\n", quoted);
        Assert.Empty(SyntaxTree.ParseText(quoted).GetDiagnostics());
    }

    [Fact]
    public void Quote_CanGenerateCSharpOutput()
    {
        var quoted = QuoterUnderTest.QuoteText("let answer = 42", new QuoterOptionsUnderTest
        {
            OutputLanguage = QuoterLanguageUnderTest.CSharp
        });

        Assert.StartsWith("using Raven.CodeAnalysis.Syntax;", quoted);
        Assert.Contains("using static Raven.CodeAnalysis.Syntax.SyntaxFactory;", quoted);
        Assert.EndsWith(".NormalizeWhitespace();\n", quoted);
    }

    [Fact]
    public void Quote_RavenOutputUsesCollectionExpressions()
    {
        var quoted = QuoterUnderTest.QuoteText("""
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
        var quoted = QuoterUnderTest.QuoteText("let answer = 42", new QuoterOptionsUnderTest
        {
            WrapInClass = true
        });

        Assert.Contains("static class QuotedSyntax {", quoted);
        Assert.Contains("static func Create() -> CompilationUnitSyntax {", quoted);
        Assert.Empty(SyntaxTree.ParseText(quoted).GetDiagnostics());
    }

    [Fact]
    public void Quote_UsesSmallestMatchingFactoryOverload()
    {
        var node = ParenthesizedExpression(IdentifierName("value"));

        var quoted = QuoterUnderTest.Quote(node, new QuoterOptionsUnderTest
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

        var quoted = QuoterUnderTest.Quote(node, new QuoterOptionsUnderTest
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

        var quoted = QuoterUnderTest.Quote(node, new QuoterOptionsUnderTest
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

        var quoted = QuoterUnderTest.Quote(node, new QuoterOptionsUnderTest
        {
            GenerateUsingDirectives = false,
            UseNamedArguments = true
        });

        Assert.Contains("ExpressionStatement(", quoted);
        Assert.Contains("expression:", quoted);
        Assert.DoesNotContain("terminatorToken:", quoted);
    }

    [Fact]
    public void Quote_CanEmitFullyQualifiedExpressionWithoutNormalization()
    {
        var quoted = QuoterUnderTest.Quote(
            SyntaxFactory.ParseExpression("left + right"),
            new QuoterOptionsUnderTest
            {
                GenerateUsingDirectives = false,
                FullyQualifyNames = true,
                NormalizeWhitespace = false
            });

        Assert.StartsWith(
            "Raven.CodeAnalysis.Syntax.SyntaxFactory.InfixOperatorExpression(",
            quoted);
        Assert.Contains(
            "Raven.CodeAnalysis.Syntax.SyntaxKind.AddExpression",
            quoted);
        Assert.DoesNotContain("NormalizeWhitespace", quoted);
        Assert.Empty(SyntaxTree.ParseText($"let quoted = {quoted}").GetDiagnostics());
    }

    [Fact]
    public void Quote_CanEmitSourceInPlaceOfSelectedNode()
    {
        var quoted = QuoterUnderTest.Quote(
            SyntaxFactory.ParseExpression("left + placeholder"),
            new QuoterOptionsUnderTest
            {
                GenerateUsingDirectives = false,
                NormalizeWhitespace = false,
                NodeSourceOverride = node => node is IdentifierNameSyntax identifier &&
                    identifier.Identifier.ValueText == "placeholder"
                        ? "replacement"
                        : null
            });

        Assert.Contains("Identifier(\"left\")", quoted);
        Assert.Contains("replacement", quoted);
        Assert.DoesNotContain("placeholder", quoted);
    }

    [Fact]
    public void Quote_SourceOverride_PreservesSurroundingTrivia()
    {
        var quoted = QuoterUnderTest.Quote(
            SyntaxFactory.ParseExpression("left + placeholder + right"),
            new QuoterOptionsUnderTest
            {
                GenerateUsingDirectives = false,
                NormalizeWhitespace = false,
                NodeSourceOverride = node => node is IdentifierNameSyntax identifier &&
                    identifier.Identifier.ValueText == "placeholder"
                        ? "replacement"
                        : null
            });

        Assert.Contains("(replacement)", quoted);
        Assert.Contains("WhitespaceTrivia", quoted);
    }
}
