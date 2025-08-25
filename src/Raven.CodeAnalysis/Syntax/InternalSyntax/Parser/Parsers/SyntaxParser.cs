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
            ReadToken();
            return true;
        }
        return false;
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
        if (ConsumeToken(SyntaxKind.NewLineToken, out token))
            return true;

        // Allow optional semicolon
        if (ConsumeToken(SyntaxKind.SemicolonToken, out token))
            return true;

        // Allow End of File
        if (ConsumeToken(SyntaxKind.EndOfFileToken, out token))
            return true;

        // Allow end of block when statement is last in a block
        if (IsNextToken(SyntaxKind.CloseBraceToken))
        {
            token = Token(SyntaxKind.None);
            return true;
        }

        // If newlines are tokens, check if it's a valid terminator
        if (TreatNewlinesAsTokens && IsNewLineToken(PeekToken()))
        {
            var previous = LastToken!;

            if (!IsInsideParens && !IsLineContinuable(previous))
            {
                token = ReadToken(); // consume newline as terminator
                return true;
            }
        }

        token = MissingToken(SyntaxKind.SemicolonToken);
        return false;
    }


    private bool IsLineContinuable(SyntaxToken token)
    {
        return token.Kind switch
        {
            SyntaxKind.PlusToken or
            SyntaxKind.MinusToken or
            SyntaxKind.StarToken or
            SyntaxKind.SlashToken or
            SyntaxKind.DotToken or
            SyntaxKind.QuestionToken or
            SyntaxKind.AmpersandToken or
            //SyntaxKind.PipeToken or
            SyntaxKind.EqualsToken or
            SyntaxKind.OpenParenToken or
            SyntaxKind.OpenBracketToken => true,
            _ => false
        };
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
