using System.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class Tokenizer : ITokenizer
{
    private readonly ILexer _lexer;
    private readonly List<SyntaxToken> _lookaheadTokens = new List<SyntaxToken>();
    private SyntaxToken _currentToken;
    private readonly StringBuilder _stringBuilder = new StringBuilder();

    public int Position { get; private set; }

    /// <summary>
    /// Treat newlines as tokens
    /// </summary>
    /// <value></value>
    public bool EmitNewlinesAsTokens { get; set; } = false;

    /// <summary>
    /// Treat LineFeedToken, CarriageReturnToken, CarriageReturnLineFeedToken, and NewlineToken, as trivia.
    /// </summary>
    /// <value></value>
    public bool TreatNewlineSequencesAsTrivia => !EmitNewlinesAsTokens;

    /// <summary>
    /// Use EndOfLineTrivia - instead of LineFeedTrivia, CarriageReturnTrivia, and CarriageReturnLineFeedTrivia.
    /// </summary>
    /// <value></value>
    public bool UseEndOfLineTrivia { get; set; } = false;

    public SyntaxKind LineFeedTriviaKind => UseEndOfLineTrivia ? SyntaxKind.EndOfLineTrivia : SyntaxKind.LineFeedTrivia;
    public SyntaxKind CarriageReturnTriviaKind => UseEndOfLineTrivia ? SyntaxKind.EndOfLineTrivia : SyntaxKind.CarriageReturnTrivia;
    public SyntaxKind CarriageReturnLineFeedTriviaKind => UseEndOfLineTrivia ? SyntaxKind.EndOfLineTrivia : SyntaxKind.CarriageReturnLineFeedTrivia;

    public Tokenizer(TextReader textReader)
    {
        _lexer = new Lexer(textReader);
    }

    public SyntaxToken ReadToken()
    {
        if (_lookaheadTokens.Count > 0)
        {
            // Remove the token from the lookahead list
            _currentToken = _lookaheadTokens[0]; // Using index from end for clarity
            _lookaheadTokens.RemoveAt(0);
        }
        else
        {
            // Fallback to reading a new token
            _currentToken = ReadTokenCore();
        }

        // Update the position
        Position += _currentToken.FullWidth;

        return _currentToken;
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
        Token token;

        SyntaxTriviaList leadingTrivia;
        SyntaxTriviaList trailingTrivia;

        leadingTrivia = ReadTrivia(isTrailingTrivia: false);

        token = _lexer.ReadToken();

        trailingTrivia = ReadTrivia(isTrailingTrivia: true);

        return new SyntaxToken(token.Kind, token.Text, token.Value, token.Length, leadingTrivia, trailingTrivia, token.GetDiagnostics());
    }

    private SyntaxTriviaList ReadTrivia(bool isTrailingTrivia)
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            if (_stringBuilder.Length > 0) _stringBuilder.Clear();

            var token = _lexer.PeekToken(0);

            if (token.Kind == SyntaxKind.EndOfFileToken)
            {
                break;
            }

            if (token.Kind == SyntaxKind.SlashToken)
            {
                var token2 = _lexer.PeekToken(1);

                if (token2.Kind == SyntaxKind.SlashToken)
                {
                    _stringBuilder.Append(token.Text);
                    _stringBuilder.Append(token2.Text);

                    _lexer.ReadTokens(2);

                    Token peeked = _lexer.PeekToken();
                    while (peeked.Kind != SyntaxKind.LineFeedToken && peeked.Kind != SyntaxKind.EndOfFileToken)
                    {
                        _lexer.ReadToken();
                        _stringBuilder.Append(peeked.Text);
                        peeked = _lexer.PeekToken();
                    }

                    trivia.Add(new SyntaxTrivia(SyntaxKind.SingleLineCommentTrivia, _stringBuilder.ToString()));
                    continue;
                }
                else if (token2.Kind == SyntaxKind.StarToken)
                {
                    _stringBuilder.Append(token.Text);
                    _stringBuilder.Append(token2.Text);

                    _lexer.ReadAndDiscardTokens(2);

                    Token peeked = _lexer.PeekToken(0);
                    Token peeked2 = _lexer.PeekToken(1);
                    while (peeked.Kind != SyntaxKind.StarToken && peeked2.Kind != SyntaxKind.SlashToken && peeked.Kind != SyntaxKind.EndOfFileToken)
                    {
                        _lexer.ReadToken();

                        _stringBuilder.Append(peeked.Text);

                        peeked = _lexer.PeekToken(0);
                        peeked2 = _lexer.PeekToken(1);

                        if (peeked.Kind == SyntaxKind.StarToken && peeked2.Kind == SyntaxKind.SlashToken)
                        {
                            _lexer.ReadAndDiscardTokens(2);

                            _stringBuilder.Append(peeked.Text);
                            _stringBuilder.Append(peeked2.Text);
                        }
                    }

                    trivia.Add(new SyntaxTrivia(SyntaxKind.MultiLineCommentTrivia, _stringBuilder.ToString()));
                    continue;
                }
            }

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
            }

            if (TreatNewlineSequencesAsTrivia)
            {
                if (token.Kind == SyntaxKind.LineFeedToken)
                {
                    // LineFeedToken

                    _lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(LineFeedTriviaKind, token.Text));

                    if (isTrailingTrivia)
                    {
                        break;
                    }
                    continue;
                }
                else if (token.Kind == SyntaxKind.CarriageReturnToken)
                {
                    // Separate CarriageReturnToken or CarriageReturnToken and LineFeedToken

                    Token peeked2;
                    do
                    {
                        _lexer.ReadToken();
                        var next = _lexer.PeekToken();
                        if (next.Kind == SyntaxKind.LineFeedToken)
                        {
                            _lexer.ReadToken();
                            trivia.Add(
                                new SyntaxTrivia(LineFeedTriviaKind, token.Text + next.Text));
                        }
                        else
                        {
                            trivia.Add(new SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, token.Text));
                        }

                        peeked2 = _lexer.PeekToken();
                    } while (peeked2.Kind == SyntaxKind.CarriageReturnToken);

                    if (isTrailingTrivia)
                    {
                        break;
                    }
                    continue;
                }
                else if (token.Kind == SyntaxKind.CarriageReturnLineFeedToken)
                {
                    // Only if lexer produces merged CarriageReturnLineFeedToken

                    _lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(CarriageReturnLineFeedTriviaKind, token.Text));

                    if (isTrailingTrivia)
                    {
                        break;
                    }
                    continue;
                }
                else if (token.Kind == SyntaxKind.EndOfFileToken)
                {
                    // Only if lexer produces EndOfFileToken

                    _lexer.ReadToken();
                    trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));

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