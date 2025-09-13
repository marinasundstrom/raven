namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

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
    protected TextSpan GetActualTextSpan(int start, SyntaxNode node)
    {
        var firstToken = node.GetFirstToken();
        return new TextSpan(start + firstToken.LeadingTrivia.Width, node.Width);
    }

    internal bool TryConsumeTerminator(out SyntaxToken token)
    {
        SetTreatNewlinesAsTokens(true);

        var current = PeekToken();

        // Fast-path: immediate terminators
        if (IsNewLineToken(current))
        {
            token = ReadToken();
            return true;
        }

        if (current.Kind == SyntaxKind.SemicolonToken)
        {
            token = ReadToken();
            return true;
        }

        if (current.Kind == SyntaxKind.EndOfFileToken || current.Kind == SyntaxKind.CloseBraceToken)
        {
            token = Token(SyntaxKind.None);
            return true;
        }

        var skippedTokens = new List<SyntaxToken>();

        while (true)
        {
            skippedTokens.Add(ReadToken());
            current = PeekToken();

            if (current.Kind == SyntaxKind.SemicolonToken)
            {
                token = ConsumeWithLeadingSkipped(skippedTokens);
                return true;
            }

            if (IsNewLineToken(current))
            {
                token = ConsumeWithLeadingSkipped(skippedTokens);
                return true;
            }

            if (current.Kind == SyntaxKind.EndOfFileToken || current.Kind == SyntaxKind.CloseBraceToken)
            {
                AddSkippedToPending(skippedTokens);
                GetBaseContext()._lookaheadTokens.Clear();
                token = Token(SyntaxKind.None);
                return true;
            }
        }
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

    private BaseParseContext GetBaseContext()
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
}
