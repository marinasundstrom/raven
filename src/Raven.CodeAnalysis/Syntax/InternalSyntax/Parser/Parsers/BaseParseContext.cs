using System.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

/// <summary>
/// The entry point for parsers
/// </summary>
internal class BaseParseContext : ParseContext
{
    internal SyntaxToken? _lastToken;
    private readonly ILexer _lexer;
    private int _position;
    private bool _treatNewlinesAsTokens;
    internal readonly List<SyntaxToken> _lookaheadTokens = new List<SyntaxToken>();
    internal readonly List<SyntaxTrivia> _pendingTrivia = new();
    private readonly StringBuilder _stringBuilder = new StringBuilder();

    public BaseParseContext(ILexer lexer, int position = 0) : base()
    {
        _lexer = lexer;
        _position = position;
    }

    public override int Position => _position;

    public override SyntaxToken? LastToken => _lastToken;

    public override TextSpan GetStartOfLastToken()
    {
        if (LastToken is null)
            throw new InvalidOperationException("Have not started parsing yet");

        return new TextSpan(Position - LastToken.FullWidth, 0);
    }

    public override TextSpan GetEndOfLastToken()
    {
        if (LastToken is null)
            throw new InvalidOperationException("Have not started parsing yet");

        return new TextSpan(Position - LastToken.TrailingTrivia.Width, 0);
    }

    public override TextSpan GetSpanOfLastToken()
    {
        if (LastToken is null)
            throw new InvalidOperationException("Have not started parsing yet");

        int tokenStart = Position - LastToken.TrailingTrivia.Width - LastToken.Width;
        return new TextSpan(tokenStart, LastToken.Width);
    }

    public override TextSpan GetFullSpanOfLastToken()
    {
        if (LastToken is null)
            throw new InvalidOperationException("Have not started parsing yet");

        return new TextSpan(Position - LastToken.FullWidth, LastToken.FullWidth);
    }

    /// <summary>
    /// Treat newlines as tokens
    /// </summary>
    /// <value></value>
    public override bool TreatNewlinesAsTokens => _treatNewlinesAsTokens;

    public override void SetTreatNewlinesAsTokens(bool value)
    {
        if (_treatNewlinesAsTokens == value)
            return;

        _treatNewlinesAsTokens = value;

        var rewindPosition = Position;
        if (value && _lastToken is { })
        {
            rewindPosition -= _lastToken.TrailingTrivia.Width;
        }

        RewindToPosition(rewindPosition);
    }

    /// <summary>
    /// Create a checkpoint that enables rewinding the token stream
    /// </summary>
    public override ParserCheckpoint CreateCheckpoint(string debugName = "")
    {
        return new ParserCheckpoint(this, debugName);
    }

    /// <summary>
    /// Rewind to a specific position in the token stream
    /// </summary>
    /// <param name="position"></param>
    public override void RewindToPosition(int position)
    {
        _lookaheadTokens.Clear(); // Invalidate lookahead because context changed
        _lexer.ResetToPosition(position);
        _position = position;
    }

    /// <summary>
    /// Treat LineFeedToken, CarriageReturnToken, CarriageReturnLineFeedToken, and NewlineToken, as trivia.
    /// </summary>
    /// <value></value>
    public bool TreatNewlineSequencesAsTrivia => !TreatNewlinesAsTokens;

    /// <summary>
    /// Use EndOfLineTrivia - instead of LineFeedTrivia, CarriageReturnTrivia, and CarriageReturnLineFeedTrivia.
    /// </summary>
    /// <value></value>
    public bool UseEndOfLineTrivia { get; set; } = true;

    public SyntaxKind LineFeedTriviaKind => UseEndOfLineTrivia ? SyntaxKind.EndOfLineTrivia : SyntaxKind.LineFeedTrivia;
    public SyntaxKind CarriageReturnTriviaKind => UseEndOfLineTrivia ? SyntaxKind.EndOfLineTrivia : SyntaxKind.CarriageReturnTrivia;
    public SyntaxKind CarriageReturnLineFeedTriviaKind => UseEndOfLineTrivia ? SyntaxKind.EndOfLineTrivia : SyntaxKind.CarriageReturnLineFeedTrivia;

    public override SyntaxToken ReadToken()
    {
        if (_lookaheadTokens.Count > 0)
        {
            // Remove the token from the lookahead list
            _lastToken = _lookaheadTokens[0]; // Using index from end for clarity
            _lookaheadTokens.RemoveAt(0);
        }
        else
        {
            // Fallback to reading a new token
            _lastToken = ReadTokenCore();
        }

        // Update the position
        _position += _lastToken.FullWidth;

        return _lastToken;
    }

    public override SyntaxToken PeekToken(int index = 0)
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
        Token token = _lexer.PeekToken();

        if (TreatNewlinesAsTokens && token.Kind == SyntaxKind.NewLineToken)
        {
            // Immediately return NewLineToken.

            _lexer.ReadToken();
            return new SyntaxToken(token.Kind, token.Text, token.Value, token.Length, SyntaxTriviaList.Empty, SyntaxTriviaList.Empty, token.GetDiagnostics());
        }

        SyntaxTriviaList leadingTrivia = ReadTrivia(isTrailingTrivia: false);

        if (_pendingTrivia.Count > 0)
        {
            leadingTrivia = new SyntaxTriviaList(_pendingTrivia.Concat(leadingTrivia.GetLeadingTrivia()).ToArray());
            _pendingTrivia.Clear();
        }

        // Check if we can extract a terminator from trivia
        var syntheticNewline = TryExtractNewlineTokenFromPendingTrivia(ref leadingTrivia);
        if (syntheticNewline != null)
            return syntheticNewline;

        token = _lexer.ReadToken();

        if (token.Kind == SyntaxKind.NewLineToken)
        {
            // Immediately return NewLineToken.

            return new SyntaxToken(token.Kind, token.Text, token.Value, token.Length, SyntaxTriviaList.Empty, SyntaxTriviaList.Empty, token.GetDiagnostics());
        }

        SyntaxTriviaList trailingTrivia = ReadTrivia(isTrailingTrivia: true);

        return new SyntaxToken(token.Kind, token.Text, token.Value, token.Length, leadingTrivia, trailingTrivia, token.GetDiagnostics());
    }

    private SyntaxToken? TryExtractNewlineTokenFromPendingTrivia(ref SyntaxTriviaList leadingTrivia)
    {
        if (!TreatNewlinesAsTokens)
            return null;

        for (int i = 0; i < leadingTrivia.Count; i++)
        {
            var trivia = leadingTrivia[i];

            if (trivia.Kind == SyntaxKind.EndOfLineTrivia && ShouldPromoteToNewlineToken())
            {
                leadingTrivia = leadingTrivia.RemoveAt(i);

                return new SyntaxToken(
                    kind: SyntaxKind.NewLineToken,
                    text: trivia.Text,
                    value: null,
                    width: trivia.Text.Length,
                    leadingTrivia: SyntaxTriviaList.Empty,
                    trailingTrivia: SyntaxTriviaList.Empty,
                    diagnostics: null
                );
            }
        }

        return null;
    }

    private bool ShouldPromoteToNewlineToken()
    {
        return TreatNewlinesAsTokens && !IsInsideParens;
    }

    private SyntaxTriviaList ReadTrivia(bool isTrailingTrivia)
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            if (_stringBuilder.Length > 0) _stringBuilder.Clear();

            var token = _lexer.PeekToken(0);

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
                    var commentTrivia = new SyntaxTrivia(SyntaxKind.SingleLineCommentTrivia, _stringBuilder.ToString());

                    if (isTrailingTrivia && _lexer.PeekToken().Kind == SyntaxKind.EndOfFileToken)
                    {
                        _pendingTrivia.Add(commentTrivia);
                        break;
                    }

                    trivia.Add(commentTrivia);
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

                    var commentTrivia = new SyntaxTrivia(SyntaxKind.MultiLineCommentTrivia, _stringBuilder.ToString());

                    if (isTrailingTrivia && _lexer.PeekToken().Kind == SyntaxKind.EndOfFileToken)
                    {
                        _pendingTrivia.Add(commentTrivia);
                        break;
                    }

                    trivia.Add(commentTrivia);
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
                else if (token.Kind == SyntaxKind.NewLineToken)
                {
                    if (isTrailingTrivia)
                        break;

                    //trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));

                    Token peeked = default!;
                    do
                    {
                        _lexer.ReadToken();
                        trivia.Add(new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, token.Text));
                        peeked = _lexer.PeekToken();
                    } while (peeked.Kind == SyntaxKind.NewLineToken);

                    continue;
                }
                else if (token.Kind == SyntaxKind.EndOfFileToken)
                {
                    break;
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

    public override SyntaxToken SkipUntil(params IEnumerable<SyntaxKind> expectedKind)
    {
        var skippedTokens = new List<SyntaxToken>();

        _pendingTrivia.Clear();

        SyntaxToken token = PeekToken();

        while (!expectedKind.Contains(token.Kind) && token.Kind != SyntaxKind.EndOfFileToken)
        {
            skippedTokens.Add(ReadToken());

            token = PeekToken();
        }

        // If we reached end-of-file, preserve the skipped tokens as trivia and return a None token
        if (token.Kind == SyntaxKind.EndOfFileToken)
        {
            if (skippedTokens.Count > 0)
            {
                var trivia = new SyntaxTrivia(
                    new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray()))
                );

                _pendingTrivia.Add(trivia);
            }

            return SyntaxFactory.Token(SyntaxKind.None);
        }

        if (skippedTokens.Count > 0)
        {
            var trivia = new SyntaxTrivia(
                new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray()))
            );

            var leadingTrivia = token.LeadingTrivia.Add(trivia);
            ReadToken();
            _lastToken = new SyntaxToken(token.Kind, token.Text, leadingTrivia, token.TrailingTrivia);
            return _lastToken;
        }

        return ReadToken();
    }
}
