using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

/// <summary>
/// The entry point for parsers
/// </summary>
internal class BaseParseContext : ParseContext
{
    internal SyntaxToken? _lastToken;
    private readonly ILexer _lexer;
    private readonly CancellationToken _cancellationToken;
    private readonly bool _emitSkippedTokensDiagnostics;
    private int _position;
    private bool _treatNewlinesAsTokens;
    internal readonly List<SyntaxToken> _lookaheadTokens = new List<SyntaxToken>();
    internal readonly List<SyntaxTrivia> _pendingTrivia = new();
    private readonly StringBuilder _stringBuilder = new StringBuilder();
    private const int MaxSkippedTokensPerTrivia = 64;

#if DEBUG
    private readonly ParserProgressWatchdog _progressWatchdog;
#endif

    public BaseParseContext(
        ILexer lexer,
        int position = 0,
        CancellationToken cancellationToken = default,
        bool emitSkippedTokensDiagnostics = false) : base()
    {
        _lexer = lexer;
        _cancellationToken = cancellationToken;
        _position = position;
        _emitSkippedTokensDiagnostics = emitSkippedTokensDiagnostics;

#if DEBUG
        _progressWatchdog = new ParserProgressWatchdog(position);
#endif
    }

    public override int Position => _position;

    public override SyntaxToken? LastToken => _lastToken;

    public override CancellationToken CancellationToken => _cancellationToken;

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
        if (value && _lastToken is { } lastToken)
        {
            int rewindBy = 0;
            int pendingWhitespace = 0;

            for (int i = lastToken.TrailingTrivia.Count - 1; i >= 0; i--)
            {
                var trivia = lastToken.TrailingTrivia[i];

                if (trivia.Kind == SyntaxKind.WhitespaceTrivia)
                {
                    pendingWhitespace += trivia.FullWidth;
                    continue;
                }

                if (IsEndOfLineTrivia(trivia))
                {
                    rewindBy += trivia.FullWidth + pendingWhitespace;
                    pendingWhitespace = 0;
                    continue;
                }

                if (rewindBy > 0)
                    break;

                pendingWhitespace = 0;
            }

            if (rewindBy > 0)
                rewindPosition -= rewindBy;
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

        ResetParserProgress($"RewindTo({position})");
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
        ThrowIfCancellationRequested();

        ObserveParserProgress("ReadToken");

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

        RecordParserAdvance();

        return _lastToken;
    }

    public override SyntaxToken PeekToken(int index = 0)
    {
        ThrowIfCancellationRequested();

        ObserveParserProgress($"PeekToken({index})");

        // Ensure the lookahead tokens list is populated up to the requested index
        while (_lookaheadTokens.Count <= index)
        {
            _lookaheadTokens.Add(ReadTokenCore());
        }

        return _lookaheadTokens[index];
    }

    private SyntaxToken ReadTokenCore()
    {
        ThrowIfCancellationRequested();

        ObserveParserProgress("ReadTokenCore");

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
            ThrowIfCancellationRequested();

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

    public override SyntaxToken SkipUntil(params SyntaxKind[] expectedKind)
    {
        var skippedTokens = new List<SyntaxToken>();
        var skippedTrivia = new List<SyntaxTrivia>();
        var skippedWidth = 0;
        var skippedSpanStart = Position;

        _pendingTrivia.Clear();

        SyntaxToken token = PeekToken();

        while (!expectedKind.Contains(token.Kind) && token.Kind != SyntaxKind.EndOfFileToken)
        {
            ThrowIfCancellationRequested();
            var skipped = ReadToken();
            skippedTokens.Add(skipped);
            skippedWidth += skipped.FullWidth;
            FlushSkippedTokenBatches(skippedTokens, skippedTrivia);

            token = PeekToken();
        }

        AddSkippedTokensDiagnostic(skippedSpanStart, skippedWidth, token.Kind, DescribeExpectedKinds(expectedKind));

        // If we reached end-of-file, preserve the skipped tokens as trivia and return a None token
        if (token.Kind == SyntaxKind.EndOfFileToken)
        {
            AddSkippedTokensAsTrivia(skippedTokens, skippedTrivia);

            if (skippedTrivia.Count > 0)
            {
                _pendingTrivia.AddRange(skippedTrivia);
            }

            return SyntaxFactory.Token(SyntaxKind.None);
        }

        AddSkippedTokensAsTrivia(skippedTokens, skippedTrivia);

        if (skippedTrivia.Count > 0)
        {
            var leadingTrivia = new SyntaxTriviaList(token.LeadingTrivia.Concat(skippedTrivia).ToArray());
            ReadToken();
            _lastToken = new SyntaxToken(token.Kind, token.Text, leadingTrivia, token.TrailingTrivia);
            return _lastToken;
        }

        return ReadToken();
    }

    internal void AddSkippedTokensDiagnostic(int spanStart, int skippedWidth, SyntaxKind recoveryKind, string expectedDescription)
    {
        if (!_emitSkippedTokensDiagnostics)
            return;

        if (skippedWidth <= 0)
            return;

        if (string.IsNullOrWhiteSpace(expectedDescription))
            expectedDescription = recoveryKind.ToString();

        AddDiagnostic(DiagnosticInfo.Create(
            CompilerDiagnostics.SkippedTokens,
            new TextSpan(spanStart, skippedWidth),
            recoveryKind.ToString(),
            expectedDescription));
    }

    internal LoopProgressTracker StartLoopProgress(string loopName)
    {
        return new LoopProgressTracker(this, loopName);
    }

    internal void EnsureLoopProgress(ref int lastObservedPosition, string loopName)
    {
        if (_position != lastObservedPosition)
        {
            lastObservedPosition = _position;
            return;
        }

        ForceAdvanceAfterStall(loopName);
        lastObservedPosition = _position;
    }

    private void ForceAdvanceAfterStall(string loopName)
    {
        var stalledToken = PeekToken();

        if (stalledToken.Kind == SyntaxKind.EndOfFileToken)
        {
            throw new InvalidOperationException($"Parser made no progress in '{loopName}' at end-of-file.");
        }

        var spanStart = _position;
        var skippedTokens = new List<SyntaxToken> { ReadToken() };
        var firstSkipped = skippedTokens[0];
        var skippedTrivia = new List<SyntaxTrivia>();

        AddSkippedTokensAsTrivia(skippedTokens, skippedTrivia);

        _pendingTrivia.AddRange(skippedTrivia);

        AddDiagnostic(DiagnosticInfo.Create(
            CompilerDiagnostics.ParserMadeNoProgress,
            new TextSpan(spanStart, firstSkipped.FullWidth),
            loopName,
            stalledToken.Kind.ToString()));
    }

    private void ThrowIfCancellationRequested()
    {
        CancellationToken.ThrowIfCancellationRequested();
    }

    internal void FlushSkippedTokenBatches(List<SyntaxToken> skippedTokens, List<SyntaxTrivia> destination)
    {
        int consumed = 0;

        while (skippedTokens.Count - consumed >= MaxSkippedTokensPerTrivia)
        {
            destination.Add(CreateSkippedTrivia(skippedTokens, consumed, MaxSkippedTokensPerTrivia));
            consumed += MaxSkippedTokensPerTrivia;
        }

        if (consumed > 0)
        {
            skippedTokens.RemoveRange(0, consumed);
        }
    }

    internal void AddSkippedTokensAsTrivia(List<SyntaxToken> skippedTokens, List<SyntaxTrivia> destination)
    {
        FlushSkippedTokenBatches(skippedTokens, destination);

        if (skippedTokens.Count == 0)
            return;

        destination.Add(CreateSkippedTrivia(skippedTokens, 0, skippedTokens.Count));
        skippedTokens.Clear();
    }

    private static SyntaxTrivia CreateSkippedTrivia(List<SyntaxToken> tokens, int start, int count)
    {
        var batch = new SyntaxToken[count];
        tokens.CopyTo(start, batch, 0, count);

        return new SyntaxTrivia(new SkippedTokensTrivia(new SyntaxList(batch)));
    }

    private static string DescribeExpectedKinds(IEnumerable<SyntaxKind> expectedKinds)
    {
        return DescribeKinds(expectedKinds);
    }

    internal static string DescribeKinds(IEnumerable<SyntaxKind> expectedKinds)
    {
        var builder = new StringBuilder();
        HashSet<SyntaxKind>? seen = null;

        foreach (var kind in expectedKinds)
        {
            seen ??= new HashSet<SyntaxKind>();

            if (!seen.Add(kind))
                continue;

            if (builder.Length > 0)
                builder.Append(", ");

            builder.Append(kind);
        }

        return builder.ToString();
    }


    [Conditional("DEBUG")]
    private void ObserveParserProgress(string location)
    {
#if DEBUG
        _progressWatchdog.Observe(_position, _lastToken, _lookaheadTokens, location);
#endif
    }

    [Conditional("DEBUG")]
    private void RecordParserAdvance()
    {
#if DEBUG
        _progressWatchdog.RecordAdvance(_position, _lastToken, _lookaheadTokens);
#endif
    }

    [Conditional("DEBUG")]
    private void ResetParserProgress(string reason)
    {
#if DEBUG
        _progressWatchdog.Reset(_position, reason);
#endif
    }
}

internal ref struct LoopProgressTracker
{
    private readonly BaseParseContext _context;
    private readonly string _loopName;
    private int _lastObservedPosition;
    private bool _hasSnapshot;

    public LoopProgressTracker(BaseParseContext context, string loopName)
    {
        _context = context;
        _loopName = loopName;
        _lastObservedPosition = context.Position;
        _hasSnapshot = false;
    }

    public void EnsureProgress()
    {
        if (!_hasSnapshot)
        {
            _lastObservedPosition = _context.Position;
            _hasSnapshot = true;
            return;
        }

        if (_context.PeekToken().Kind == SyntaxKind.EndOfFileToken)
        {
            return;
        }

        _context.EnsureLoopProgress(ref _lastObservedPosition, _loopName);
    }
}

#if DEBUG
internal sealed class ParserProgressWatchdog
{
    private readonly Stopwatch _stopwatch = Stopwatch.StartNew();
    private readonly List<string> _locations = new();
    private int _lastObservedPosition;
    private long _lastAdvanceTimestamp;
    private int _stallIterations;

    private const int MaxIterationsWithoutProgress = 2048;
    private const int MinStallMilliseconds = 250;
    private const int MaxLocationTrail = 12;

    public ParserProgressWatchdog(int initialPosition)
    {
        _lastObservedPosition = initialPosition;
        _lastAdvanceTimestamp = _stopwatch.ElapsedMilliseconds;
    }

    public void Observe(int position, SyntaxToken? lastToken, IReadOnlyList<SyntaxToken> lookahead, string location)
    {
        if (position != _lastObservedPosition)
        {
            RecordAdvance(position, lastToken, lookahead);
            return;
        }

        _stallIterations++;
        RecordLocation(location);

        long elapsed = _stopwatch.ElapsedMilliseconds - _lastAdvanceTimestamp;
        if (_stallIterations < MaxIterationsWithoutProgress || elapsed < MinStallMilliseconds)
            return;

        var message = new StringBuilder();
        message.Append("Parser progress watchdog detected a stall: ")
            .Append(_stallIterations)
            .Append(" iterations without advancing beyond position ")
            .Append(position)
            .Append(" after ")
            .Append(elapsed)
            .Append("ms. ");

        message.Append("Last token: ").Append(DescribeToken(lastToken)).Append(". ");
        message.Append("Lookahead: ").Append(DescribeLookahead(lookahead)).Append(". ");
        message.Append("Recent calls: ").Append(string.Join(" -> ", _locations)).Append('.');

        throw new InvalidOperationException(message.ToString());
    }

    public void RecordAdvance(int position, SyntaxToken? lastToken, IReadOnlyList<SyntaxToken> lookahead)
    {
        _lastObservedPosition = position;
        _lastAdvanceTimestamp = _stopwatch.ElapsedMilliseconds;
        _stallIterations = 0;
        _locations.Clear();
        RecordLocation($"advance:{DescribeToken(lastToken)}|{DescribeLookahead(lookahead)}");
    }

    public void Reset(int position, string reason)
    {
        _lastObservedPosition = position;
        _lastAdvanceTimestamp = _stopwatch.ElapsedMilliseconds;
        _stallIterations = 0;
        _locations.Clear();
        RecordLocation($"reset:{reason}@{position}");
    }

    private void RecordLocation(string location)
    {
        if (_locations.Count == MaxLocationTrail)
            _locations.RemoveAt(0);

        _locations.Add(location);
    }

    private static string DescribeToken(SyntaxToken? token)
    {
        if (token is null)
            return "<none>";

        var text = token.Text;
        if (text.Length > 16)
            text = text[..16] + "…";

        return $"{token.Kind} ({token.FullWidth}w:'{text}')";
    }

    private static string DescribeLookahead(IReadOnlyList<SyntaxToken> lookahead)
    {
        if (lookahead.Count == 0)
            return "<empty>";

        var previewCount = Math.Min(3, lookahead.Count);
        var parts = new string[previewCount];

        for (int i = 0; i < previewCount; i++)
        {
            parts[i] = DescribeToken(lookahead[i]);
        }

        return string.Join(", ", parts);
    }
}
#endif
