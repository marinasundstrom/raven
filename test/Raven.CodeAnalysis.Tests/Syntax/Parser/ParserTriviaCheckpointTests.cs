using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Shouldly;

using InternalSyntaxToken = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserTriviaCheckpointTests
{
    [Fact]
    public void SingleLineCommentTrivia_PreservedAcrossCheckpoint()
    {
        const string code = "let x = 1; // trailing";
        var context = CreateContextAndReadToToken(code, SyntaxKind.SemicolonToken);

        var positionBeforeCheckpoint = context.Position;
        context.LastToken.ShouldNotBeNull();
        context.LastToken!.Kind.ShouldBe(SyntaxKind.SemicolonToken);

        var checkpoint = context.CreateCheckpoint();
        {
            var endOfFile = context.ReadToken();
            endOfFile.Kind.ShouldBe(SyntaxKind.EndOfFileToken);

            AssertTrivia(endOfFile, SyntaxKind.SingleLineCommentTrivia, "// trailing");

            context.LastToken.ShouldNotBeNull();
            context.LastToken!.Kind.ShouldBe(SyntaxKind.EndOfFileToken);
            context.Position.ShouldBeGreaterThan(positionBeforeCheckpoint);
        }

        checkpoint.Rewind();

        context.Position.ShouldBe(positionBeforeCheckpoint);
        context.LastToken.ShouldNotBeNull();
        context.LastToken!.Kind.ShouldBe(SyntaxKind.SemicolonToken);

        var endOfFileAfterReset = context.ReadToken();
        endOfFileAfterReset.Kind.ShouldBe(SyntaxKind.EndOfFileToken);
        AssertTrivia(endOfFileAfterReset, SyntaxKind.SingleLineCommentTrivia, "// trailing");
        context.LastToken.ShouldNotBeNull();
        context.LastToken!.Kind.ShouldBe(SyntaxKind.EndOfFileToken);
    }

    [Fact]
    public void MultiLineCommentTrivia_PreservedAcrossCheckpoint()
    {
        const string code = "let x = 1; /* trailing */";
        var context = CreateContextAndReadToToken(code, SyntaxKind.SemicolonToken);

        var positionBeforeCheckpoint = context.Position;
        context.LastToken.ShouldNotBeNull();
        context.LastToken!.Kind.ShouldBe(SyntaxKind.SemicolonToken);

        var checkpoint = context.CreateCheckpoint();
        {
            var endOfFile = context.ReadToken();
            endOfFile.Kind.ShouldBe(SyntaxKind.EndOfFileToken);

            AssertTrivia(endOfFile, SyntaxKind.MultiLineCommentTrivia, "/* trailing */");

            context.LastToken.ShouldNotBeNull();
            context.LastToken!.Kind.ShouldBe(SyntaxKind.EndOfFileToken);
            context.Position.ShouldBeGreaterThan(positionBeforeCheckpoint);
        }

        checkpoint.Rewind();

        context.Position.ShouldBe(positionBeforeCheckpoint);
        context.LastToken.ShouldNotBeNull();
        context.LastToken!.Kind.ShouldBe(SyntaxKind.SemicolonToken);

        var endOfFileAfterReset = context.ReadToken();
        endOfFileAfterReset.Kind.ShouldBe(SyntaxKind.EndOfFileToken);
        AssertTrivia(endOfFileAfterReset, SyntaxKind.MultiLineCommentTrivia, "/* trailing */");
        context.LastToken.ShouldNotBeNull();
        context.LastToken!.Kind.ShouldBe(SyntaxKind.EndOfFileToken);
    }

    [Fact]
    public void Diagnostics_AddedDuringSpeculation_AreRolledBackOnCheckpointRewind()
    {
        const string code = "(logger as ConsoleLogger)";
        var context = new BaseParseContext(new Lexer(new StringReader(code)));

        context.Diagnostics.ShouldBeEmpty();

        var checkpoint = context.CreateCheckpoint();
        {
            var parameterList = new StatementSyntaxParser(context).ParseParameterList();
            parameterList.ShouldNotBeNull();
            context.Diagnostics.ShouldNotBeEmpty();
        }

        checkpoint.Rewind();

        context.Diagnostics.ShouldBeEmpty();
    }

    [Fact]
    public void LexerDiagnostics_AddedDuringSpeculation_AreRolledBackOnCheckpointRewind()
    {
        const string code = "\"";
        var context = new BaseParseContext(new Lexer(new StringReader(code)));

        context.Diagnostics.ShouldBeEmpty();

        var checkpoint = context.CreateCheckpoint();
        {
            context.ReadToken();
            context.Diagnostics.ShouldNotBeEmpty();
        }

        checkpoint.Rewind();

        context.Diagnostics.ShouldBeEmpty();
    }

    private static BaseParseContext CreateContextAndReadToToken(string code, SyntaxKind target)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(code)));
        ReadThroughToken(context, target);
        return context;
    }

    private static void ReadThroughToken(BaseParseContext context, SyntaxKind target)
    {
        while (true)
        {
            var token = context.ReadToken();
            if (token.Kind == target)
                return;

            if (token.Kind == SyntaxKind.EndOfFileToken)
                throw new InvalidOperationException($"Token '{target}' was not found before end of file.");
        }
    }

    private static void AssertTrivia(InternalSyntaxToken token, SyntaxKind expectedKind, string expectedText)
    {
        var trivia = token.LeadingTrivia.Single(t => t.Kind == expectedKind);
        trivia.Kind.ShouldBe(expectedKind);
        trivia.Text.ShouldBe(expectedText);
    }
}
