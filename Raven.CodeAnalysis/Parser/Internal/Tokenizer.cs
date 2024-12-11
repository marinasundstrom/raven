using Raven.CodeAnalysis.Syntax.InternalSyntax;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;

namespace Raven.CodeAnalysis.Parser.Internal;

public class Tokenizer
{
    private readonly ILexer lexer;
    private SyntaxToken? lookaheadToken;

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
            return token;
        }
        return ReadTokenCore();
    }

    public SyntaxToken PeekToken()
    {
        if (lookaheadToken == null)
        {
            lookaheadToken = ReadTokenCore();
        }
        return lookaheadToken!;
    }

    private SyntaxToken ReadTokenCore()
    {
        SyntaxToken token;

        SyntaxTriviaList leadingTrivia;
        SyntaxTriviaList trailingTrivia;

        leadingTrivia = ReadTrivia();

        token = lexer.ReadToken();

        trailingTrivia = ReadTrivia();

        return new SyntaxToken(token.Kind, token.Text, leadingTrivia, trailingTrivia);
    }

    private SyntaxTriviaList ReadTrivia()
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            var token = lexer.PeekToken();

            switch (token)
            {
                case SyntaxToken it when it.Kind == SyntaxKind.TabToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.TabTrivia, token.Text));
                    continue;

                case SyntaxToken it when it.Kind == SyntaxKind.Whitespace:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, token.Text));
                    continue;

                case SyntaxToken it when it.Kind == SyntaxKind.EndOfLineToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
                    continue;

                case SyntaxToken it when it.Kind == SyntaxKind.CarriageReturnToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, token.Text));
                    continue;
            }

            break;
        }

        return new SyntaxTriviaList(trivia.ToArray());
    }

    private bool ConsumeToken(SyntaxKind kind, out SyntaxToken token)
    {
        token = lexer.PeekToken();
        if (token.Kind == kind)
        {
            lexer.ReadToken();
            return true;
        }
        return false;
    }
}