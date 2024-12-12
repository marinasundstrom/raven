using Raven.CodeAnalysis.Syntax.InternalSyntax;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;
using SyntaxToken = Raven.CodeAnalysis.Syntax.SyntaxToken;

namespace Raven.CodeAnalysis.Parser.Internal;

public class Tokenizer
{
    private readonly ILexer lexer;
    private Syntax.InternalSyntax.SyntaxToken? lookaheadToken;

    public Tokenizer(ILexer lexer)
    {
        this.lexer = lexer;
    }

    public SyntaxToken ReadToken()
    {
        if (lookaheadToken != null)
        {
            var token = lookaheadToken;
            lookaheadToken = null;
            return CreateRedToken(token);
        }
        var readToken = ReadTokenCore();
        return CreateRedToken(readToken);
    }

    private static SyntaxToken CreateRedToken(Syntax.InternalSyntax.SyntaxToken readToken)
    {
        return new SyntaxToken(readToken, null!, 0);
    }

    public SyntaxToken PeekToken()
    {
        if (lookaheadToken == null)
        {
            lookaheadToken = ReadTokenCore();
        }
        return CreateRedToken(lookaheadToken);
    }

    private Syntax.InternalSyntax.SyntaxToken ReadTokenCore()
    {
        Syntax.InternalSyntax.SyntaxToken token;

        SyntaxTriviaList leadingTrivia;
        SyntaxTriviaList trailingTrivia;

        leadingTrivia = ReadTrivia();

        token = lexer.ReadToken();

        trailingTrivia = ReadTrivia();

        return new Syntax.InternalSyntax.SyntaxToken(token.Kind, token.Text, leadingTrivia, trailingTrivia);
    }

    private SyntaxTriviaList ReadTrivia()
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            var token = lexer.PeekToken();

            switch (token)
            {
                case Syntax.InternalSyntax.SyntaxToken it when it.Kind == SyntaxKind.TabToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.TabTrivia, token.Text));
                    continue;

                case Syntax.InternalSyntax.SyntaxToken it when it.Kind == SyntaxKind.Whitespace:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, token.Text));
                    continue;

                case Syntax.InternalSyntax.SyntaxToken it when it.Kind == SyntaxKind.EndOfLineToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
                    continue;

                case Syntax.InternalSyntax.SyntaxToken it when it.Kind == SyntaxKind.CarriageReturnToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, token.Text));
                    continue;
            }

            break;
        }

        return new SyntaxTriviaList(trivia.ToArray());
    }
}