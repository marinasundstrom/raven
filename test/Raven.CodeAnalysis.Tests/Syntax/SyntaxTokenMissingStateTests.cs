using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class SyntaxTokenMissingStateTests
{
    [Fact]
    public void MissingTokenStaysMissingAcrossTriviaAndMetadataCopies()
    {
        var token = SyntaxFactory.MissingToken(SyntaxKind.IdentifierToken).Green;
        var trivia = InternalSyntax.SyntaxTriviaList.Empty;
        var diagnostic = InternalSyntax.DiagnosticInfo.Create(CompilerDiagnostics.IdentifierExpected, new TextSpan(0, 0));
        var copies = new GreenNode[]
        {
            token.WithLeadingTrivia(trivia),
            token.WithTrailingTrivia(trivia),
            token.WithRawKind((int)SyntaxKind.IdentifierToken),
            token.WithAdditionalDiagnostics(diagnostic),
            token.With([], annotations: [new SyntaxAnnotation("tag")])
        };

        foreach (var copy in copies)
        {
            Assert.True(copy.IsMissing);
            Assert.Equal(0, copy.Width);
            Assert.Equal(SyntaxKind.IdentifierToken, copy.Kind);
        }
        Assert.True(token.IsMissing);
    }

    [Fact]
    public void PresentZeroWidthTokensDoNotBecomeMissing()
    {
        var token = SyntaxFactory.Token(SyntaxKind.EndOfFileToken).Green;
        Assert.False(token.IsMissing);
        Assert.False(token.WithLeadingTrivia(InternalSyntax.SyntaxTriviaList.Empty).IsMissing);
    }
}
