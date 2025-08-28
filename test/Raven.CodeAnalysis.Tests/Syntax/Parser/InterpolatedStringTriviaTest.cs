using IS = Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using System.Reflection;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class InterpolatedStringTriviaTest
{
    [Fact]
    public void InterpolatedString_PreservesLeadingAndTrailingTrivia()
    {
        var leadingTrivia = IS.SyntaxTriviaList.Create(new[] { IS.SyntaxFactory.Whitespace("  ") });
        var trailingTrivia = IS.SyntaxTriviaList.Create(new[] { IS.SyntaxFactory.Whitespace("  ") });
        var token = new IS.SyntaxToken(SyntaxKind.StringLiteralToken, "$\"foo{bar}\"", leadingTrivia, trailingTrivia);

        var parser = new ExpressionSyntaxParser(new BaseParseContext(new Lexer(new StringReader(string.Empty))));
        var method = typeof(ExpressionSyntaxParser).GetMethod("ParseInterpolatedStringExpression", BindingFlags.Instance | BindingFlags.NonPublic);
        var green = (IS.InterpolatedStringExpressionSyntax)method!.Invoke(parser, new object[] { token, "foo{bar}" })!;
        var interpolated = (InterpolatedStringExpressionSyntax)green.CreateRed()!;

        var startTrivia = interpolated.StringStartToken.LeadingTrivia;
        startTrivia.Count.ShouldBe(1);
        var leading = startTrivia.Single();
        leading.Kind.ShouldBe(SyntaxKind.WhitespaceTrivia);
        leading.ToString().ShouldBe("  ");
        interpolated.StringStartToken.TrailingTrivia.ShouldBeEmpty();

        var endTrivia = interpolated.StringEndToken.TrailingTrivia;
        endTrivia.Count.ShouldBe(1);
        var trailing = endTrivia.Single();
        trailing.Kind.ShouldBe(SyntaxKind.WhitespaceTrivia);
        trailing.ToString().ShouldBe("  ");
        interpolated.StringEndToken.LeadingTrivia.ShouldBeEmpty();
    }
}
