namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Diagnostics.CodeAnalysis;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class SyntaxParser : ParseContext
{
    public SyntaxParser(ParseContext parent) : base(parent)
    {

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

    protected enum NewlineMode
    {
        TreatAsStatementSeparator,
        IgnoreInsideExpression,
        AutoDetectContinuation
    }

    protected NewlineMode CurrentNewlineMode { get; private set; } = NewlineMode.TreatAsStatementSeparator;

    protected void EnterExpressionContext()
    {
        CurrentNewlineMode = NewlineMode.IgnoreInsideExpression;
    }

    protected void EnterStatementContext()
    {
        CurrentNewlineMode = NewlineMode.TreatAsStatementSeparator;
    }

    protected bool IsRealStatementTerminator()
    {
        var current = PeekToken();

        if (current.Kind == SyntaxKind.SemicolonToken)
            return true;

        if (current.Kind == SyntaxKind.NewLineToken)
        {
            // Lookahead to see if it's continuation
            var next = PeekToken(1);

            if (next.Kind is SyntaxKind.DotToken or
                               SyntaxKind.OpenParenToken or
                               SyntaxKind.OpenBracketToken or
                               SyntaxKind.PlusToken or
                               SyntaxKind.MinusToken or
                               SyntaxKind.StarToken or
                               SyntaxKind.SlashToken)
            {
                return false; // it's a continuation
            }

            return true; // it's a terminator
        }

        return false;
    }
}
