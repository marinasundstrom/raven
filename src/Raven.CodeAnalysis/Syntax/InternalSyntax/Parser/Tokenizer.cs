namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class Tokenizer : ITokenizer
{
    private readonly ILexer _lexer;
    private readonly List<SyntaxToken> _lookaheadTokens = new List<SyntaxToken>();

    public int Position { get; private set; }

    public Tokenizer(TextReader textReader)
    {
        _lexer = new Lexer(textReader);
    }

    public SyntaxToken ReadToken()
    {
        SyntaxToken token;

        if (_lookaheadTokens.Count > 0)
        {
            // Remove the token from the lookahead list
            token = _lookaheadTokens[0]; // Using index from end for clarity
            _lookaheadTokens.RemoveAt(0);
        }
        else
        {
            // Fallback to reading a new token
            token = ReadTokenCore();
        }

        // Update the position
        Position += token.FullWidth;

        return token;
    }

    public SyntaxToken PeekToken(int index = 0)
    {
        // Ensure the lookahead tokens list is populated up to the requested index
        while (_lookaheadTokens.Count <= index)
        {
            _lookaheadTokens.Add(ReadTokenCore());
        }

        return _lookaheadTokens[index];
    }

    private SyntaxToken ReadTokenCore()
    {
        SyntaxToken token;

        SyntaxTriviaList leadingTrivia;
        SyntaxTriviaList trailingTrivia;

        leadingTrivia = ReadTrivia(isTrailingTrivia: false);

        token = _lexer.ReadToken();

        trailingTrivia = ReadTrivia(isTrailingTrivia: true);

        return new SyntaxToken(token.Kind, token.Text, leadingTrivia, trailingTrivia, token._diagnostics);
    }

    private SyntaxTriviaList ReadTrivia(bool isTrailingTrivia)
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            var token = _lexer.PeekToken();

            switch (token.Kind)
            {
                case SyntaxKind.TabToken:
                    _lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.TabTrivia, token.Text));
                    continue;

                case SyntaxKind.Whitespace:
                    _lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, token.Text));
                    continue;

                case SyntaxKind.EndOfLineToken:
                    {
                        SyntaxToken peeked;
                        do
                        {
                            _lexer.ReadToken();
                            trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
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
                        SyntaxToken peeked2;
                        do
                        {
                            _lexer.ReadToken();
                            var next = _lexer.PeekToken();
                            if (next.Kind == SyntaxKind.EndOfLineToken)
                            {
                                _lexer.ReadToken();
                                trivia.Add(
                                    new SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, token.Text + next.Text));
                            }
                            else
                            {
                                trivia.Add(new SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, token.Text));
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

        return new SyntaxTriviaList(trivia.ToArray());
    }
}