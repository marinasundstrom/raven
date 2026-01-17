namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
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

        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.PublicKeyword or
                     SyntaxKind.PrivateKeyword or
                     SyntaxKind.InternalKeyword or
                     SyntaxKind.ProtectedKeyword or
                     SyntaxKind.StaticKeyword or
                     SyntaxKind.AbstractKeyword or
                     SyntaxKind.FinalKeyword or
                     SyntaxKind.SealedKeyword or
                     SyntaxKind.PartialKeyword or
                     SyntaxKind.RecordKeyword or
                     SyntaxKind.VirtualKeyword or
                     SyntaxKind.AsyncKeyword or
                     SyntaxKind.OpenKeyword or
                     SyntaxKind.OverrideKeyword or
                     SyntaxKind.NewKeyword)
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

    protected SyntaxToken ParseOverloadableOperatorToken()
    {
        var token = PeekToken();

        if (SyntaxFacts.IsOverloadableOperatorToken(token.Kind))
            return ReadToken();

        AddDiagnostic(
            DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetSpanOfLastToken(),
                "operator"));

        return MissingToken(SyntaxKind.PlusToken);
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

    public TextSpan GetSpanOfPeekedToken()
    {
        var peekedToken = PeekToken();
        var lastToken = GetFullSpanOfLastToken();
        var fullEnd = lastToken.End;
        fullEnd += peekedToken.LeadingTrivia.FullWidth;
        return new TextSpan(fullEnd, peekedToken.Width);
    }

    internal bool TryConsumeTerminator(out SyntaxToken token)
    {
        bool previous = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        var current = PeekToken();

        // Fast-path: immediate terminators
        if (IsNewLineToken(current))
        {
            token = ReadToken();
            SetTreatNewlinesAsTokens(previous);
            return true;
        }

        if (current.Kind == SyntaxKind.SemicolonToken)
        {
            token = ReadToken();
            SetTreatNewlinesAsTokens(previous);
            return true;
        }

        if (current.Kind == SyntaxKind.EndOfFileToken || current.Kind == SyntaxKind.CloseBraceToken)
        {
            token = Token(SyntaxKind.None);
            SetTreatNewlinesAsTokens(previous);
            return true;
        }

        if (IsPotentialStatementStart(current))
        {
            token = Token(SyntaxKind.None);
            SetTreatNewlinesAsTokens(previous);
            return true;
        }

        var skippedTokens = new List<SyntaxToken>();

        while (true)
        {
            var t = ReadToken();
            if (skippedTokens.Count == 0 && t.LeadingTrivia.Count > 0)
                t = t.WithLeadingTrivia(Array.Empty<SyntaxTrivia>());
            skippedTokens.Add(t);
            current = PeekToken();

            if (IsPotentialStatementStart(current))
            {
                AddSkippedToPending(skippedTokens);
                token = Token(SyntaxKind.None);
                SetTreatNewlinesAsTokens(previous);
                return true;
            }

            if (current.Kind == SyntaxKind.SemicolonToken)
            {
                token = ConsumeWithLeadingSkipped(skippedTokens);
                SetTreatNewlinesAsTokens(previous);
                return true;
            }

            if (IsNewLineToken(current))
            {
                token = ConsumeWithLeadingSkipped(skippedTokens);
                SetTreatNewlinesAsTokens(previous);
                return true;
            }

            if (current.Kind == SyntaxKind.EndOfFileToken || current.Kind == SyntaxKind.CloseBraceToken)
            {
                AddSkippedToPending(skippedTokens);
                GetBaseContext()._lookaheadTokens.Clear();
                token = Token(SyntaxKind.None);
                SetTreatNewlinesAsTokens(previous);
                return true;
            }
        }
    }

    private static bool IsPotentialStatementStart(SyntaxToken token)
    {
        return token.Kind switch
        {
            SyntaxKind.None => false,
            SyntaxKind.EndOfFileToken => false,
            SyntaxKind.CloseBraceToken => false,
            SyntaxKind.CloseParenToken => false,
            SyntaxKind.CloseBracketToken => false,
            SyntaxKind.SemicolonToken => false,
            SyntaxKind.CommaToken => false,
            SyntaxKind.ColonToken => false,
            SyntaxKind.NewLineToken => false,
            SyntaxKind.LineFeedToken => false,
            SyntaxKind.CarriageReturnToken => false,
            SyntaxKind.CarriageReturnLineFeedToken => false,
            _ => true,
        };
    }

    private SyntaxToken ConsumeWithLeadingSkipped(List<SyntaxToken> skippedTokens)
    {
        var baseContext = GetBaseContext();
        var terminator = PeekToken();
        ReadToken();

        if (skippedTokens.Count > 0)
        {
            var trivia = new SyntaxTrivia(
                new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray()))
            );

            var leadingTrivia = terminator.LeadingTrivia.Add(trivia);
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
        if (skippedTokens.Count == 0)
            return;

        var trivia = new SyntaxTrivia(
            new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray()))
        );

        GetBaseContext()._pendingTrivia.Add(trivia);
    }

    protected List<SyntaxToken> ConsumeSkippedTokensUntil(Func<SyntaxToken, bool> shouldStop)
    {
        var skippedTokens = new List<SyntaxToken>();

        while (true)
        {
            var current = ReadToken();

            if (skippedTokens.Count == 0 && current.LeadingTrivia.Count > 0)
                current = current.WithLeadingTrivia(Array.Empty<SyntaxTrivia>());

            skippedTokens.Add(current);

            var next = PeekToken();
            if (next.Kind == SyntaxKind.EndOfFileToken || shouldStop(next))
                break;
        }

        return skippedTokens;
    }

    protected SyntaxToken CreateSkippedToken(List<SyntaxToken> skippedTokens, TextSpan diagnosticSpan)
    {
        if (skippedTokens.Count > 0)
        {
            var unexpectedToken = skippedTokens[0];
            var unexpectedTokenDisplay = string.IsNullOrEmpty(unexpectedToken.Text)
                ? unexpectedToken.Kind.ToString()
                : unexpectedToken.Text;

            if (unexpectedToken.IsKind(SyntaxKind.CloseBraceToken))
            {
                AddDiagnostic(DiagnosticInfo.Create(CompilerDiagnostics.UnmatchedCharacter, diagnosticSpan, '}'));
            }
            else if (!unexpectedToken.IsKind(SyntaxKind.EndOfFileToken))
            {
                AddDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                    diagnosticSpan,
                    unexpectedTokenDisplay));
            }
        }

        var trivia = new SyntaxTrivia(new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray())));

        return new SyntaxToken(
            SyntaxKind.None,
            string.Empty,
            SyntaxTriviaList.Create([trivia]),
            SyntaxTriviaList.Empty);
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
