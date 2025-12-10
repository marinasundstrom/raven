namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class SyntaxParser : ParseContext
{
    public SyntaxParser(ParseContext parent) : base(parent)
    {

    }

    private int _parenDepth = 0;

    public void EnterParens() => _parenDepth++;
    public void ExitParens() => _parenDepth--;
    public bool IsInsideParens => _parenDepth > 0;

    protected static bool IsIdentifierToken(SyntaxToken token)
    {
        return token.Kind == SyntaxKind.IdentifierToken;
    }

    protected SyntaxList ParseTypeMemberModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        var loopProgress = GetBaseContext().StartLoopProgress("ParseTypeMemberModifiers");

        while (true)
        {
            loopProgress.EnsureProgress();

            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.PublicKeyword or
                     SyntaxKind.PrivateKeyword or
                     SyntaxKind.InternalKeyword or
                     SyntaxKind.ProtectedKeyword or
                     SyntaxKind.StaticKeyword or
                     SyntaxKind.AbstractKeyword or
                     SyntaxKind.SealedKeyword or
                     SyntaxKind.PartialKeyword or
                     SyntaxKind.VirtualKeyword or
                     SyntaxKind.AsyncKeyword or
                     SyntaxKind.OpenKeyword or
                     SyntaxKind.OverrideKeyword)
            {
                modifiers = modifiers.Add(ReadToken());
            }
            else
            {
                break;
            }
        }

        return modifiers;
    }

    protected static bool HasLeadingEndOfLineTrivia(SyntaxToken token)
    {
        return token.LeadingTrivia.Any(x => x.IsKind(SyntaxKind.EndOfLineTrivia));
    }

    protected static bool CanTokenBeIdentifier(SyntaxToken token)
    {
        return SyntaxFacts.CanBeIdentifier(token.Kind);
    }

    protected static SyntaxToken ToIdentifierToken(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.IdentifierToken)
            return token;

        return new SyntaxToken(
            SyntaxKind.IdentifierToken,
            token.Text,
            token.LeadingTrivia,
            token.TrailingTrivia,
            token._diagnostics,
            token._annotations);
    }

    protected void UpdateLastToken(SyntaxToken token)
    {
        GetBaseContext()._lastToken = token;
    }

    public bool IsNextToken(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = PeekToken();
        if (token.Kind == kind)
        {
            return true;
        }
        return false;
    }

    public bool IsNextToken(SyntaxKind kind)
    {
        var token = PeekToken();
        if (token.Kind == kind)
        {
            return true;
        }
        return false;
    }

    public bool ConsumeToken(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = PeekToken();

        if (token.Kind == kind)
        {
            token = ReadToken();
            return true;
        }

        return false;
    }

    protected SyntaxToken ReadIdentifierToken()
    {
        var token = ReadToken();
        if (CanTokenBeIdentifier(token))
        {
            token = ToIdentifierToken(token);
            UpdateLastToken(token);
        }

        return token;
    }

    public bool ConsumeTokenOrMissing(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        var hasConsumedToken = ConsumeToken(kind, out token);
        if (!hasConsumedToken)
        {
            token = MissingToken(kind);
            return false;
        }
        return true;
    }

    public bool ConsumeTokenOrNull(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken? token)
    {
        var hasConsumedToken = ConsumeToken(kind, out var t);
        if (!hasConsumedToken)
        {
            token = null;
            return false;
        }
        token = t;
        return true;
    }

    public SyntaxToken ExpectToken(SyntaxKind kind)
    {
        if (ConsumeToken(kind, out var token))
            return token;

        var missing = MissingToken(kind);
        return missing;
    }

    protected SyntaxToken ExpectTokenWithError(SyntaxKind kind, char expectedCharacter)
    {
        if (ConsumeToken(kind, out var token))
            return token;

        var missing = MissingToken(kind);
        AddDiagnostic(
            DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetEndOfLastToken(),
                [expectedCharacter]));

        return missing;
    }

    /// <summary>
    /// Get the actual span of a node.
    /// </summary>
    /// <param name="start">The start fullwidth</param>
    /// <param name="node">The given node</param>
    /// <returns>The actual text span</returns>
    protected TextSpan GetActualTextSpan(int start, SyntaxNode? node)
    {
        if (node is null)
        {
            return new TextSpan(Math.Max(0, start), 0);
        }

        var firstToken = node.GetFirstToken();
        var lastToken = node.GetLastToken();

        var leadingWidth = firstToken?.LeadingTrivia.Width ?? 0;
        var trailingWidth = lastToken?.TrailingTrivia.Width ?? 0;

        var fullWidth = node.FullWidth;
        var length = fullWidth - leadingWidth - trailingWidth;

        if (length < 0)
        {
            length = Math.Max(0, node.Width);
        }

        var spanStart = start + leadingWidth;

        if (spanStart < 0)
        {
            spanStart = 0;
        }

        if (length < 0)
        {
            length = 0;
        }

        return new TextSpan(spanStart, length);
    }

    internal bool TryConsumeTerminator(out SyntaxToken token)
    {
        bool previous = TreatNewlinesAsTokens;

        if (!previous)
        {
            var lookahead = PeekToken();
            if (ContainsBlankLine(lookahead.LeadingTrivia) && IsPotentialStatementStart(lookahead))
            {
                token = CreateMissingTerminatorToken();
                return true;
            }
        }

        if (LastToken is { TrailingTrivia: var trailingTrivia } && ContainsAnyNewline(trailingTrivia))
        {
            token = Token(SyntaxKind.None);
            return true;
        }

        SetTreatNewlinesAsTokens(true);

        var current = PeekToken();

        if (IsStatementBoundary(current.Kind))
        {
            // Fast-path: immediate terminators
            if (ParserRecoverySets.IsStatementTerminator(current.Kind))
            {
                token = ReadToken();
                SetTreatNewlinesAsTokens(previous);
                return true;
            }

            var hasNewlineBoundary = HasNewlineBoundary(current.LeadingTrivia);
            if (hasNewlineBoundary || current.Kind is SyntaxKind.CloseBraceToken or SyntaxKind.EndOfFileToken)
            {
                token = Token(SyntaxKind.None);
                SetTreatNewlinesAsTokens(previous);
                return true;
            }

            if (ParserRecoverySets.IsTypeMemberStartOrRecovery(current.Kind)
                && IsPotentialStatementStart(current))
            {
                token = CreateMissingTerminatorToken();
                SetTreatNewlinesAsTokens(previous);
                return true;
            }

            token = CreateMissingTerminatorToken();
            SetTreatNewlinesAsTokens(previous);
            return true;
        }

        var skippedTokens = new List<SyntaxToken>();

        var loopProgress = GetBaseContext().StartLoopProgress("TryConsumeTerminator");

        while (true)
        {
            loopProgress.EnsureProgress();

            var t = ReadToken();
            if (skippedTokens.Count == 0 && t.LeadingTrivia.Count > 0)
                t = t.WithLeadingTrivia(Array.Empty<SyntaxTrivia>());
            skippedTokens.Add(t);
            current = PeekToken();

            if (IsStatementBoundary(current.Kind))
            {
                if (ParserRecoverySets.IsStatementTerminator(current.Kind))
                {
                    token = ConsumeWithLeadingSkipped(skippedTokens);
                }
                else if (HasNewlineBoundary(current.LeadingTrivia)
                    || current.Kind is SyntaxKind.CloseBraceToken or SyntaxKind.EndOfFileToken
                    || ParserRecoverySets.IsTypeMemberStartOrRecovery(current.Kind))
                {
                    token = Token(SyntaxKind.None);
                }
                else
                {
                    AddSkippedToPending(skippedTokens);
                    GetBaseContext()._lookaheadTokens.Clear();
                    token = CreateMissingTerminatorToken();
                }

                SetTreatNewlinesAsTokens(previous);
                return true;
            }
        }
    }

    private SyntaxToken CreateMissingTerminatorToken()
    {
        return MissingToken(SyntaxKind.SemicolonToken);
    }

    private bool HasNewlineBoundary(SyntaxTriviaList leadingTrivia)
    {
        if (ContainsAnyNewline(leadingTrivia))
            return true;

        if (LastToken is { TrailingTrivia: var trailing } && ContainsAnyNewline(trailing))
            return true;

        return false;
    }

    private static bool ContainsBlankLine(SyntaxTriviaList trivia)
    {
        int lineBreaks = 0;

        foreach (var triviaItem in trivia)
        {
            if (triviaItem.Kind is SyntaxKind.EndOfLineTrivia or SyntaxKind.LineFeedTrivia or SyntaxKind.CarriageReturnTrivia or SyntaxKind.CarriageReturnLineFeedTrivia)
            {
                lineBreaks++;

                if (lineBreaks > 1)
                    return true;
            }
            else if (triviaItem.Kind != SyntaxKind.WhitespaceTrivia)
            {
                lineBreaks = 0;
            }
        }

        return false;
    }

    private static bool ContainsAnyNewline(SyntaxTriviaList trivia)
    {
        foreach (var triviaItem in trivia)
        {
            if (triviaItem.Kind is SyntaxKind.EndOfLineTrivia
                or SyntaxKind.LineFeedTrivia
                or SyntaxKind.CarriageReturnTrivia
                or SyntaxKind.CarriageReturnLineFeedTrivia)
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsStatementBoundary(SyntaxKind kind)
    {
        return ParserRecoverySets.IsStatementRecovery(kind)
            || ParserRecoverySets.IsTypeMemberStartOrRecovery(kind);
    }

    private static bool IsPotentialStatementStart(SyntaxToken token)
    {
        return !ParserRecoverySets.IsStatementRecovery(token.Kind);
    }

    private SyntaxToken ConsumeWithLeadingSkipped(List<SyntaxToken> skippedTokens)
    {
        var baseContext = GetBaseContext();
        var terminator = PeekToken();
        ReadToken();

        var skippedTrivia = new List<SyntaxTrivia>();
        baseContext.AddSkippedTokensAsTrivia(skippedTokens, skippedTrivia);

        if (skippedTrivia.Count > 0)
        {
            var leadingTrivia = new SyntaxTriviaList(terminator.LeadingTrivia.Concat(skippedTrivia).ToArray());
            var newToken = new SyntaxToken(
                terminator.Kind,
                terminator.Text,
                terminator.GetValue(),
                terminator.Width,
                leadingTrivia,
                terminator.TrailingTrivia);

            baseContext._lastToken = newToken;
            return newToken;
        }

        return terminator;
    }

    private void AddSkippedToPending(List<SyntaxToken> skippedTokens)
    {
        var baseContext = GetBaseContext();
        baseContext.AddSkippedTokensAsTrivia(skippedTokens, baseContext._pendingTrivia);
    }

    protected SyntaxToken SkipBadTokensUntil(IReadOnlyCollection<SyntaxKind> recoveryKinds)
    {
        var baseContext = GetBaseContext();
        var skippedTokens = new List<SyntaxToken>();
        var skippedTrivia = new List<SyntaxTrivia>();
        var loopProgress = baseContext.StartLoopProgress("SkipBadTokensUntil");
        var skippedSpanStart = Position;
        var skippedWidth = 0;

        var token = PeekToken();
        while (!recoveryKinds.Contains(token.Kind) && token.Kind != SyntaxKind.EndOfFileToken)
        {
            loopProgress.EnsureProgress();
            var skipped = ReadToken();
            skippedTokens.Add(skipped);
            skippedWidth += skipped.FullWidth;
            baseContext.FlushSkippedTokenBatches(skippedTokens, skippedTrivia);
            token = PeekToken();
        }

        baseContext.AddSkippedTokensAsTrivia(skippedTokens, skippedTrivia);

        baseContext.AddSkippedTokensDiagnostic(
            skippedSpanStart,
            skippedWidth,
            token.Kind,
            BaseParseContext.DescribeKinds(recoveryKinds));

        if (skippedTrivia.Count > 0)
            baseContext._pendingTrivia.AddRange(skippedTrivia);

        if (token.Kind == SyntaxKind.EndOfFileToken)
            return Token(SyntaxKind.None);

        return token;
    }

    protected BaseParseContext GetBaseContext()
    {
        ParseContext ctx = this;
        while (ctx is not BaseParseContext)
        {
            ctx = ctx.Parent!;
        }

        return (BaseParseContext)ctx;
    }

    internal LoopProgressTracker StartLoopProgress(string loopName)
    {
        return GetBaseContext().StartLoopProgress(loopName);
    }

    private static bool IsNewLineToken(SyntaxToken token)
    {
        return token.Kind is
            SyntaxKind.LineFeedToken or
            SyntaxKind.CarriageReturnToken or
            SyntaxKind.CarriageReturnLineFeedToken or
            SyntaxKind.NewLineToken;
    }

    private static SyntaxKind GetTriviaKindForNewLineToken(BaseParseContext baseContext, SyntaxToken newlineToken)
    {
        return newlineToken.Text switch
        {
            "\r\n" => baseContext.CarriageReturnLineFeedTriviaKind,
            "\r" => baseContext.CarriageReturnTriviaKind,
            "\n" => baseContext.LineFeedTriviaKind,
            _ => baseContext.LineFeedTriviaKind,
        };
    }
}
