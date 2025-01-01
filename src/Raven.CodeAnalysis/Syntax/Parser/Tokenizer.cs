using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax.Parser;

internal class Tokenizer : ITokenizer
{
    private readonly ILexer _lexer;
    private InternalSyntax.SyntaxToken? _lookaheadToken;

    public Tokenizer(TextReader textReader, List<InternalDiagnostic> diagnostics)
    {
        _lexer = new Lexer(textReader, diagnostics);
    }

    public SyntaxToken ReadToken()
    {
        if (_lookaheadToken != null)
        {
            var token = _lookaheadToken;
            _lookaheadToken = null;
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
        if (_lookaheadToken == null)
        {
            _lookaheadToken = ReadTokenCore();
        }
        return CreateRedToken(_lookaheadToken);
    }

    private InternalSyntax.SyntaxToken ReadTokenCore()
    {
        InternalSyntax.SyntaxToken token;

        InternalSyntax.SyntaxTriviaList leadingTrivia;
        InternalSyntax.SyntaxTriviaList trailingTrivia;

        leadingTrivia = ReadTrivia(isTrailingTrivia: false);

        token = _lexer.ReadToken();

        trailingTrivia = ReadTrivia(isTrailingTrivia: true);

        return new InternalSyntax.SyntaxToken(token.Kind, token.Text, leadingTrivia, trailingTrivia);
    }

    private InternalSyntax.SyntaxTriviaList ReadTrivia(bool isTrailingTrivia)
    {
        List<InternalSyntax.SyntaxTrivia> trivia = [];

        while (true)
        {
            var token = _lexer.PeekToken();

            switch (token.Kind)
            {
                case SyntaxKind.TabToken:
                    _lexer.ReadToken();
                    trivia.Add(new InternalSyntax.SyntaxTrivia(SyntaxKind.TabTrivia, token.Text));
                    continue;
                case SyntaxKind.Whitespace:
                    _lexer.ReadToken();
                    trivia.Add(new InternalSyntax.SyntaxTrivia(SyntaxKind.WhitespaceTrivia, token.Text));
                    continue;

                case SyntaxKind.EndOfLineToken:
                    {
                        InternalSyntax.SyntaxToken peeked;
                        do
                        {
                            _lexer.ReadToken();
                            trivia.Add(new InternalSyntax.SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
                            peeked = _lexer.PeekToken();
                        } while (peeked.Kind == SyntaxKind.EndOfLineToken);

                        if (isTrailingTrivia)
                        {
                            break;
                        }
                        continue;
                    }
                case SyntaxKind.CarriageReturnToken:
                    {
                        InternalSyntax.SyntaxToken peeked2;
                        do
                        {
                            _lexer.ReadToken();
                            var next = _lexer.PeekToken();
                            if (next.Kind == SyntaxKind.EndOfLineToken)
                            {
                                _lexer.ReadToken();
                                trivia.Add(
                                    new InternalSyntax.SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, token.Text + next.Text));
                            }
                            else
                            {
                                trivia.Add(new InternalSyntax.SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, token.Text));
                            }

                            peeked2 = _lexer.PeekToken();
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

        return new InternalSyntax.SyntaxTriviaList(trivia.ToArray());
    }
}
