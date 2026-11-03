using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class BaseParseContextStateTests
{
    [Theory]
    [InlineData("let x = 1; // trailing")]
    [InlineData("let x = 1; /* trailing */")]
    [InlineData("/// first\n/// second\nclass C {}")]
    [InlineData("\tlet x = 1\r\n\r\nlet y = 2\n")]
    public void TokenStream_PreservesTriviaText(string source)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(source)));
        var text = new System.Text.StringBuilder();
        for (var count = 0; count < 100; count++)
        {
            var token = context.ReadToken();
            foreach (var trivia in token.LeadingTrivia) text.Append(trivia.Text);
            text.Append(token.Text);
            foreach (var trivia in token.TrailingTrivia) text.Append(trivia.Text);
            if (token.Kind == SyntaxKind.EndOfFileToken)
            {
                Assert.Equal(source, text.ToString());
                Assert.Equal(source.Length, context.Position);
                return;
            }
        }
        Assert.True(false, "Token stream did not reach EOF.");
    }

    [Fact]
    public void Checkpoint_RestoresPositionBlockDepthLastTokenAndDiagnostics()
    {
        var context = new BaseParseContext(new Lexer(new StringReader("{ value }")));
        context.PeekToken(2);
        Assert.Equal(0, context.Position);
        Assert.Equal(0, context.BlockDepth);
        Assert.Null(context.LastToken);
        var open = context.ReadToken();
        var position = context.Position;
        Assert.Equal(1, context.BlockDepth);
        var checkpoint = context.CreateCheckpoint("body");
        Assert.Equal("body", checkpoint.DebugName);
        context.ReadToken();
        context.ReadToken();
        context.AddDiagnostic(DiagnosticInfo.Create(CompilerDiagnostics.InvalidExpressionTerm, new TextSpan(0, 1), "probe"));
        Assert.Equal(0, context.BlockDepth);
        checkpoint.Rewind();
        Assert.Equal(position, context.Position);
        Assert.Equal(1, context.BlockDepth);
        Assert.Same(open, context.LastToken);
        Assert.Empty(context.Diagnostics);
        Assert.Equal("value", context.ReadToken().Text);
    }

    [Fact]
    public void Checkpoint_DisposeDoesNotRewind()
    {
        var context = new BaseParseContext(new Lexer(new StringReader("first second")));
        var checkpoint = context.CreateCheckpoint();
        context.ReadToken();
        var position = context.Position;
        checkpoint.Dispose();
        Assert.Equal(position, context.Position);
        Assert.Equal("second", context.ReadToken().Text);
    }

    [Fact]
    public void Checkpoint_RestoresPendingEndOfFileComment()
    {
        var context = new BaseParseContext(new Lexer(new StringReader("x; // trailing")));
        context.ReadToken();
        context.ReadToken();
        var checkpoint = context.CreateCheckpoint();
        var first = context.ReadToken();
        checkpoint.Rewind();
        var second = context.ReadToken();
        Assert.Equal(SyntaxKind.EndOfFileToken, second.Kind);
        Assert.Equal(first.LeadingTrivia.Select(t => t.Text), second.LeadingTrivia.Select(t => t.Text));
        Assert.Contains(second.LeadingTrivia, t => t.Text == "// trailing");
    }
}
