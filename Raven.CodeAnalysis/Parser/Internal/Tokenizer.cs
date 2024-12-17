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

        leadingTrivia = ReadTrivia(isTrailingTrivia: false);

        token = lexer.ReadToken();

        trailingTrivia = ReadTrivia(isTrailingTrivia: true);

        return new Syntax.InternalSyntax.SyntaxToken(token.Kind, token.Text, leadingTrivia, trailingTrivia);
    }

    private SyntaxTriviaList ReadTrivia(bool isTrailingTrivia)
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            var token = lexer.PeekToken();

            switch (token.Kind)
            {
                case SyntaxKind.TabToken:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.TabTrivia, token.Text));
                    continue;
                case SyntaxKind.Whitespace:
                    lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, token.Text));
                    continue;

                case SyntaxKind.EndOfLineToken:
                    {
                        Syntax.InternalSyntax.SyntaxToken peeked;
                        do
                        {
                            lexer.ReadToken();
                            trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
                            peeked = lexer.PeekToken();
                        } while (peeked.Kind == SyntaxKind.EndOfLineToken);

                        if (isTrailingTrivia)
                        {
                            break;
                        }
                        continue;
                    }
                case SyntaxKind.CarriageReturnToken:
                    {
                        Syntax.InternalSyntax.SyntaxToken peeked2;
                        do
                        {
                            lexer.ReadToken();
                            var next = lexer.PeekToken();
                            if (next.Kind == SyntaxKind.EndOfLineToken)
                            {
                                lexer.ReadToken();
                                trivia.Add(
                                    new SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, token.Text + next.Text));
                            }
                            else
                            {
                                trivia.Add(new SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, token.Text));
                            }

                            peeked2 = lexer.PeekToken();
                        } while (peeked2.Kind == SyntaxKind.CarriageReturnToken);

                        if (isTrailingTrivia)
                        {
                            break;
                        }
                        continue;
                    }
            }

            break;
        }

        return new SyntaxTriviaList(trivia.ToArray());
    }
}