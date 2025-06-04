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
                    while (!IsNewLine(peeked) && peeked.Kind != SyntaxKind.EndOfFileToken)
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

                    while (true)
                    {
                        var current = _lexer.PeekToken(0);
                        var next = _lexer.PeekToken(1);

                        if (current.Kind == SyntaxKind.StarToken && next.Kind == SyntaxKind.SlashToken)
                        {
                            _lexer.ReadAndDiscardTokens(2);
                            _stringBuilder.Append(current.Text);
                            _stringBuilder.Append(next.Text);
                            break;
                        }

                        if (current.Kind == SyntaxKind.EndOfFileToken)
                        {
                            // Unterminated block comment
                            _lexer.ReadToken();
                            _stringBuilder.Append(current.Text);
                            break;
                        }

                        _lexer.ReadToken();
                        _stringBuilder.Append(current.Text);
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

                    Token peeked = default!;
                    do
                    {
                        _lexer.ReadToken();
                        trivia.Add(new SyntaxTrivia(LineFeedTriviaKind, token.Text));
                        peeked = _lexer.PeekToken();
                    } while (peeked.Kind == SyntaxKind.LineFeedToken);

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

                    Token peeked = default!;
                    do
                    {
                        _lexer.ReadToken();
                        trivia.Add(new SyntaxTrivia(CarriageReturnLineFeedTriviaKind, token.Text));
                        peeked = _lexer.PeekToken();
                    } while (peeked.Kind == SyntaxKind.CarriageReturnLineFeedToken);

                    if (isTrailingTrivia)
                    {
                        break;
                    }
                    continue;
                }
                else if (token.Kind == SyntaxKind.EndOfFileToken)
                {
                    // Only if lexer produces EndOfFileToken

                    Token peeked = default!;
                    do
                    {
                        _lexer.ReadToken();
                        trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
                        peeked = _lexer.PeekToken();
                    } while (peeked.Kind == SyntaxKind.EndOfFileToken);

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

    private static bool IsNewLine(Token token)
    {
        return token.Kind == SyntaxKind.LineFeedToken || token.Kind == SyntaxKind.NewLineToken;
    }
}