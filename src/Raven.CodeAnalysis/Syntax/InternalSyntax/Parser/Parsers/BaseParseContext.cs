using System;
using System.Collections.Generic;
using System.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

/// <summary>
/// The entry point for parsers
/// </summary>
internal class BaseParseContext : ParseContext
{
    internal SyntaxToken? _lastToken;
    private readonly ILexer _lexer;
    private readonly ParseOptions _options;
    private int _position;
    internal int _blockDepth;
    private bool _treatNewlinesAsTokens;
    internal readonly List<SyntaxToken> _lookaheadTokens = new List<SyntaxToken>();
    internal readonly List<SyntaxTrivia> _pendingTrivia = new();
    private readonly StringBuilder _stringBuilder = new StringBuilder();

    public BaseParseContext(ILexer lexer, ParseOptions options, int position = 0) : base()
    {
        _lexer = lexer;
        _options = options ?? new ParseOptions();
        _position = position;
    }

    public BaseParseContext(ILexer lexer, int position = 0)
        : this(lexer, new ParseOptions(), position)
    {
    }

    public override int Position => _position;

    public ParseOptions Options => _options;

    public override SyntaxToken? LastToken => _lastToken;

    public override int BlockDepth => _blockDepth;

    public override bool IsInBlock => _blockDepth > 0;

    public override TextSpan GetStartOfLastToken()
    {
        if (LastToken is null)
            throw new InvalidOperationException("Have not started parsing yet");

        return new TextSpan(Position - LastToken.FullWidth, 0);
    }

    public override TextSpan GetEndOfLastToken(int width = 0)
    {
        if (LastToken is null)
            throw new InvalidOperationException("Have not started parsing yet");

        return new TextSpan(Position - LastToken.TrailingTrivia.Width, width);
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

        RewindToPosition(Position);

        if (value && _lastToken is { } lastToken)
        {
            StageTrailingNewlinesForPendingTrivia(lastToken);
        }
    }

    private void StageTrailingNewlinesForPendingTrivia(SyntaxToken lastToken)
    {
        var trailing = lastToken.TrailingTrivia;

        if (trailing.Count == 0)
            return;

        var triviaToStage = new List<SyntaxTrivia>();

        for (int i = trailing.Count - 1; i >= 0; i--)
        {
            var trivia = trailing[i];

            if (trivia.Kind == SyntaxKind.WhitespaceTrivia)
            {
                triviaToStage.Insert(0, trivia);
                continue;
            }

            if (IsEndOfLineTrivia(trivia))
            {
                triviaToStage.Insert(0, trivia);

                for (int before = i - 1; before >= 0; before--)
                {
                    var precedingTrivia = trailing[before];

                    if (precedingTrivia.Kind != SyntaxKind.WhitespaceTrivia)
                        break;

                    triviaToStage.Insert(0, precedingTrivia);
                }

                break;
            }

            break;
        }

        var newlineTrivia = triviaToStage.FirstOrDefault(IsEndOfLineTrivia);

        if (newlineTrivia == default)
            return;

        var leadingTrivia = triviaToStage
            .Where(t => t != newlineTrivia)
            .ToArray();

        var newlineToken = new SyntaxToken(
            kind: SyntaxKind.NewLineToken,
            text: newlineTrivia.Text,
            value: null,
            width: newlineTrivia.Text.Length,
            leadingTrivia: leadingTrivia.Length > 0 ? new SyntaxTriviaList(leadingTrivia) : SyntaxTriviaList.Empty,
            trailingTrivia: SyntaxTriviaList.Empty,
            diagnostics: null);

        _position -= newlineToken.FullWidth;

        if (_position < 0)
            _position = 0;

        _lookaheadTokens.Insert(0, newlineToken);
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

    private static bool IsEndOfLineTrivia(InternalSyntax.SyntaxTrivia trivia)
    {
        return trivia.Kind is SyntaxKind.EndOfLineTrivia
            or SyntaxKind.LineFeedTrivia
            or SyntaxKind.CarriageReturnTrivia
            or SyntaxKind.CarriageReturnLineFeedTrivia;
    }

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

        TrackBlockDepth(_lastToken);

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

        var encounteredDocumentationTrivia = false;

        for (int i = 0; i < leadingTrivia.Count; i++)
        {
            var trivia = leadingTrivia[i];

            encounteredDocumentationTrivia |= trivia.Kind == SyntaxKind.DocumentationCommentTrivia;

            if (encounteredDocumentationTrivia)
                continue;

            if (trivia.Kind == SyntaxKind.EndOfLineTrivia && ShouldPromoteToNewlineToken())
            {
                // Preserve any documentation comments or other trivia for the next token
                // instead of losing them when we surface a newline token.
                for (int before = 0; before < i; before++)
                {
                    _pendingTrivia.Add(leadingTrivia[before]);
                }

                for (int after = i + 1; after < leadingTrivia.Count; after++)
                {
                    _pendingTrivia.Add(leadingTrivia[after]);
                }

                leadingTrivia = SyntaxTriviaList.Empty;

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

    private void TrackBlockDepth(SyntaxToken token)
    {
        switch (token.Kind)
        {
            case SyntaxKind.OpenBraceToken:
                _blockDepth++;
                break;
            case SyntaxKind.CloseBraceToken:
                if (_blockDepth > 0)
                    _blockDepth--;
                break;
        }
    }

    private SyntaxTriviaList ReadTrivia(bool isTrailingTrivia)
    {
        List<SyntaxTrivia> trivia = [];

        while (true)
        {
            if (_stringBuilder.Length > 0) _stringBuilder.Clear();

            var token = _lexer.PeekToken(0);

            // Comments are now lexed as single tokens; convert them directly to trivia.
            // DocumentationCommentTrivia represents one or more consecutive "///" lines.
            if (token.Kind == SyntaxKind.DocumentationCommentTrivia)
            {
                // Doc comments only attach as leading trivia.
                if (isTrailingTrivia)
                    break;

                List<SyntaxTrivia>? newlineTrivia = TreatNewlinesAsTokens ? [] : null;
                ReadDocumentationCommentBlockInto(_stringBuilder, newlineTrivia);

                var docTrivia = new SyntaxTrivia(SyntaxKind.DocumentationCommentTrivia, _stringBuilder.ToString());
                trivia.Add(docTrivia);

                if (newlineTrivia is { Count: > 0 })
                    trivia.AddRange(newlineTrivia);

                continue;
            }
            else if (token.Kind == SyntaxKind.SingleLineCommentTrivia)
            {
                _lexer.ReadToken();
                var commentTrivia = new SyntaxTrivia(SyntaxKind.SingleLineCommentTrivia, token.Text);

                if (isTrailingTrivia && _lexer.PeekToken().Kind == SyntaxKind.EndOfFileToken)
                {
                    _pendingTrivia.Add(commentTrivia);
                    break;
                }

                trivia.Add(commentTrivia);
                continue;
            }
            else if (token.Kind == SyntaxKind.MultiLineCommentTrivia)
            {
                _lexer.ReadToken();
                var commentTrivia = new SyntaxTrivia(SyntaxKind.MultiLineCommentTrivia, token.Text);

                if (isTrailingTrivia && _lexer.PeekToken().Kind == SyntaxKind.EndOfFileToken)
                {
                    _pendingTrivia.Add(commentTrivia);
                    break;
                }

                trivia.Add(commentTrivia);
                continue;
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

    private void ReadDocumentationCommentBlockInto(StringBuilder sb, List<SyntaxTrivia>? newlineTrivia)
    {
        while (true)
        {
            ConsumeIndentationInto(sb);

            var comment = _lexer.PeekToken(0);
            if (comment.Kind != SyntaxKind.DocumentationCommentTrivia)
                return;

            _lexer.ReadToken();
            sb.Append(comment.Text);

            // The documentation comment token already contains the whole line up to (but not including) the newline.
            var t = _lexer.PeekToken();
            while (!IsNewLine(t) && t.Kind != SyntaxKind.EndOfFileToken)
            {
                _lexer.ReadToken();
                sb.Append(t.Text);
                t = _lexer.PeekToken();
            }

            if (t.Kind == SyntaxKind.EndOfFileToken)
                return;

            var newline = ConsumeOneNewlineInto(sb);
            if (newlineTrivia is { } && newline is { } newlineValue)
                newlineTrivia.Add(newlineValue);

            if (!NextLineStartsWithDocComment())
                return;
        }
    }

    private void ConsumeIndentationInto(StringBuilder sb)
    {
        while (true)
        {
            var t = _lexer.PeekToken(0);
            if (t.Kind == SyntaxKind.Whitespace || t.Kind == SyntaxKind.TabToken)
            {
                _lexer.ReadToken();
                sb.Append(t.Text);
                continue;
            }

            break;
        }
    }

    private bool NextLineStartsWithDocComment()
    {
        int i = 0;
        while (true)
        {
            var t = _lexer.PeekToken(i);
            if (t.Kind == SyntaxKind.Whitespace || t.Kind == SyntaxKind.TabToken) { i++; continue; }
            break;
        }

        var a = _lexer.PeekToken(i + 0);
        return a.Kind == SyntaxKind.DocumentationCommentTrivia;
    }

    private SyntaxTrivia? ConsumeOneNewlineInto(StringBuilder sb)
    {
        var t = _lexer.PeekToken(0);

        if (t.Kind == SyntaxKind.CarriageReturnLineFeedToken)
        {
            _lexer.ReadToken();
            sb.Append(t.Text);

            return new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, t.Text);
        }

        if (t.Kind == SyntaxKind.CarriageReturnToken)
        {
            _lexer.ReadToken();
            sb.Append(t.Text);

            var lf = _lexer.PeekToken(0);
            if (lf.Kind == SyntaxKind.LineFeedToken)
            {
                _lexer.ReadToken();
                sb.Append(lf.Text);

                return new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, t.Text + lf.Text);
            }

            return new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, t.Text);
        }

        if (t.Kind == SyntaxKind.LineFeedToken || t.Kind == SyntaxKind.NewLineToken)
        {
            _lexer.ReadToken();
            sb.Append(t.Text);

            return new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, t.Text);
        }

        return null;
    }

    private static bool IsNewLine(Token token)
    {
        return token.Kind is SyntaxKind.NewLineToken
            or SyntaxKind.LineFeedToken
            or SyntaxKind.CarriageReturnToken
            or SyntaxKind.CarriageReturnLineFeedToken;
    }

    public override SyntaxToken SkipUntil(params IEnumerable<SyntaxKind> expectedKind)
    {
        var skippedTokens = new List<SyntaxToken>();

        _pendingTrivia.Clear();

        SyntaxToken token = PeekToken();

        bool IsBlockRecoveryStopper(SyntaxToken t) => IsInBlock && t.Kind == SyntaxKind.CloseBraceToken;

        while (!expectedKind.Contains(token.Kind)
            && token.Kind != SyntaxKind.EndOfFileToken
            && !IsBlockRecoveryStopper(token))
        {
            skippedTokens.Add(ReadToken());

            token = PeekToken();
        }

        // If we reached end-of-file, preserve the skipped tokens as trivia and return a None token
        if (token.Kind == SyntaxKind.EndOfFileToken || IsBlockRecoveryStopper(token))
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
