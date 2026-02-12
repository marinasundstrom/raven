using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Raven.CodeAnalysis.Syntax;

public sealed class SyntaxNormalizer : SyntaxRewriter
{
    private readonly string _indent;
    private int _indentLevel;
    private int _pendingLineBreaks;
    private bool _isFirstToken = true;
    private SyntaxToken _previousToken;

    public SyntaxNormalizer(int indentSize = 4)
    {
        _indent = new string(' ', indentSize);
    }

    public TSyntax Visit<TSyntax>(TSyntax syntax)
        where TSyntax : SyntaxNode
    {
        ResetState();

        var tokens = syntax.DescendantTokens().ToArray();
        if (tokens.Length == 0)
        {
            return syntax;
        }

        var replacements = new Dictionary<SyntaxToken, SyntaxToken>(tokens.Length);
        foreach (var token in tokens)
        {
            replacements[token] = NormalizeToken(token);
        }

        return (TSyntax)syntax.ReplaceTokens(tokens, (original, _) =>
            replacements.TryGetValue(original, out var replacement) ? replacement : original);
    }

    public override SyntaxToken VisitToken(SyntaxToken token)
    {
        return NormalizeToken(token);
    }

    private void ResetState()
    {
        _indentLevel = 0;
        _pendingLineBreaks = 0;
        _isFirstToken = true;
        _previousToken = default;
    }

    private SyntaxToken NormalizeToken(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.None || token is { IsMissing: true, FullWidth: 0 })
        {
            return token;
        }

        if (IsNewLineToken(token.Kind))
        {
            var normalizedNewLine = SyntaxFactory.NewLineToken
                .WithLeadingTrivia(SyntaxFactory.TriviaList())
                .WithTrailingTrivia(SyntaxFactory.TriviaList());

            TrackTokenFlow(normalizedNewLine);
            return normalizedNewLine;
        }

        if (HasNonWhitespaceTrivia(token.LeadingTrivia) || HasNonWhitespaceTrivia(token.TrailingTrivia))
        {
            TrackTokenFlow(token);
            return token;
        }

        var effectiveIndent = _indentLevel;
        if (token.Kind == SyntaxKind.CloseBraceToken && effectiveIndent > 0)
        {
            effectiveIndent--;
            _indentLevel = effectiveIndent;
        }

        var leading = CreateLeadingTrivia(token, effectiveIndent);
        var trailing = SyntaxFactory.TriviaList();

        token = token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing);

        TrackTokenFlow(token);

        return token;
    }

    private void TrackTokenFlow(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.OpenBraceToken)
        {
            _indentLevel++;
        }

        _pendingLineBreaks = GetPendingLineBreaksAfter(token);
        _previousToken = token;
        _isFirstToken = false;
    }

    private SyntaxTriviaList CreateLeadingTrivia(SyntaxToken token, int effectiveIndent)
    {
        if (_isFirstToken)
        {
            return SyntaxFactory.TriviaList();
        }

        if (token.Kind == SyntaxKind.EndOfFileToken || IsNewLineToken(token.Kind))
        {
            return SyntaxFactory.TriviaList();
        }

        var lineBreaks = _pendingLineBreaks;
        if (ShouldAttachToPreviousToken(token))
        {
            lineBreaks = 0;
        }
        else if (token.Kind == SyntaxKind.CloseBraceToken
            && _previousToken.Kind != SyntaxKind.OpenBraceToken
            && !IsNewLineToken(_previousToken.Kind))
        {
            lineBreaks = Math.Max(lineBreaks, 1);
        }

        if (lineBreaks > 0)
        {
            var trivias = new List<SyntaxTrivia>(lineBreaks + 1);
            for (var i = 0; i < lineBreaks; i++)
            {
                trivias.Add(SyntaxFactory.LineFeed);
            }

            if (token.Kind != SyntaxKind.EndOfFileToken)
            {
                var indentText = string.Concat(Enumerable.Repeat(_indent, effectiveIndent));
                if (indentText.Length > 0)
                {
                    trivias.Add(SyntaxFactory.Whitespace(indentText));
                }
            }

            return SyntaxFactory.TriviaList(trivias);
        }

        return NeedsSpace(_previousToken, token)
            ? SyntaxFactory.TriviaList(SyntaxFactory.Space)
            : SyntaxFactory.TriviaList();
    }

    private static bool HasNonWhitespaceTrivia(SyntaxTriviaList triviaList)
    {
        foreach (var trivia in triviaList)
        {
            if (!IsWhitespaceTrivia(trivia.Kind))
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsWhitespaceTrivia(SyntaxKind kind)
    {
        return kind is SyntaxKind.WhitespaceTrivia
            or SyntaxKind.TabTrivia
            or SyntaxKind.LineFeedTrivia
            or SyntaxKind.CarriageReturnTrivia
            or SyntaxKind.CarriageReturnLineFeedTrivia
            or SyntaxKind.EndOfLineTrivia;
    }

    private bool ShouldAttachToPreviousToken(SyntaxToken token)
    {
        return _previousToken.Kind == SyntaxKind.CloseBraceToken
            && token.Kind is SyntaxKind.ElseKeyword or SyntaxKind.CatchKeyword or SyntaxKind.FinallyKeyword;
    }

    private int GetPendingLineBreaksAfter(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.EndOfFileToken)
        {
            return 0;
        }

        if (IsNewLineToken(token.Kind))
        {
            return 1;
        }

        if (token.Kind == SyntaxKind.SemicolonToken)
        {
            return 1;
        }

        if (token.Kind == SyntaxKind.OpenBraceToken)
        {
            return 1;
        }

        if (token.Kind == SyntaxKind.CloseBraceToken)
        {
            return 1;
        }

        if (IsImplicitLineBreakToken(token))
        {
            return 1;
        }

        return 0;
    }

    private bool IsImplicitLineBreakToken(SyntaxToken token)
    {
        if (token.Parent is null)
        {
            return false;
        }

        if (token.Parent.GetLastToken(includeZeroWidth: true) != token)
        {
            return false;
        }

        // Keep directive/member forms and top-level statements line-oriented.
        return token.Parent.Kind is SyntaxKind.ImportDirective
            or SyntaxKind.AliasDirective
            or SyntaxKind.GlobalStatement
            or SyntaxKind.EnumMemberDeclaration
            or SyntaxKind.UnionCaseClause;
    }

    private bool NeedsSpace(SyntaxToken previous, SyntaxToken current)
    {
        if (previous.Kind == SyntaxKind.None || current.Kind == SyntaxKind.None)
        {
            return false;
        }

        if (current.Kind == SyntaxKind.EndOfFileToken)
        {
            return false;
        }

        if (IsNoSpaceBefore(current.Kind) || IsNoSpaceAfter(previous.Kind))
        {
            return false;
        }

        if (IsDotPair(previous.Kind, current.Kind))
        {
            return false;
        }

        if (previous.Kind == SyntaxKind.CloseBraceToken
            && current.Kind is SyntaxKind.ElseKeyword or SyntaxKind.CatchKeyword or SyntaxKind.FinallyKeyword)
        {
            return true;
        }

        if (IsOperator(previous) || IsOperator(current))
        {
            if (IsUnaryOperatorToken(previous) || IsUnaryOperatorToken(current))
            {
                return false;
            }

            return true;
        }

        if (previous.Kind == SyntaxKind.CommaToken)
        {
            return true;
        }

        if (previous.Kind == SyntaxKind.ColonToken)
        {
            return true;
        }

        if (current.Kind == SyntaxKind.OpenParenToken)
        {
            return previous.Kind is SyntaxKind.IfKeyword
                or SyntaxKind.ForKeyword
                or SyntaxKind.WhileKeyword
                or SyntaxKind.CatchKeyword
                or SyntaxKind.MatchKeyword;
        }

        if (current.Kind == SyntaxKind.OpenBraceToken)
        {
            return previous.Kind != SyntaxKind.DollarToken;
        }

        if (IsWord(previous.Kind) && IsWord(current.Kind))
        {
            return true;
        }

        return false;
    }

    private static bool IsNoSpaceBefore(SyntaxKind kind)
    {
        return kind is SyntaxKind.CommaToken
            or SyntaxKind.SemicolonToken
            or SyntaxKind.CloseParenToken
            or SyntaxKind.CloseBracketToken
            or SyntaxKind.CloseBraceToken
            or SyntaxKind.DotToken
            or SyntaxKind.ColonToken
            or SyntaxKind.QuestionToken
            or SyntaxKind.NewLineToken
            or SyntaxKind.LineFeedToken
            or SyntaxKind.CarriageReturnToken
            or SyntaxKind.CarriageReturnLineFeedToken;
    }

    private static bool IsNoSpaceAfter(SyntaxKind kind)
    {
        return kind is SyntaxKind.OpenParenToken
            or SyntaxKind.OpenBracketToken
            or SyntaxKind.OpenBraceToken
            or SyntaxKind.DotToken;
    }

    private static bool IsDotPair(SyntaxKind previous, SyntaxKind current)
    {
        return (previous == SyntaxKind.QuestionToken && current == SyntaxKind.DotToken)
            || (previous == SyntaxKind.DotToken && current == SyntaxKind.DotToken);
    }

    private static bool IsNewLineToken(SyntaxKind kind)
    {
        return kind is SyntaxKind.NewLineToken
            or SyntaxKind.LineFeedToken
            or SyntaxKind.CarriageReturnToken
            or SyntaxKind.CarriageReturnLineFeedToken;
    }

    private bool IsOperator(SyntaxToken token)
    {
        if (token.Kind is SyntaxKind.LessThanToken or SyntaxKind.GreaterThanToken)
        {
            return !IsTypeArgumentDelimiter(token);
        }

        return token.Kind is SyntaxKind.EqualsToken
            or SyntaxKind.EqualsEqualsToken
            or SyntaxKind.NotEqualsToken
            or SyntaxKind.LessThanOrEqualsToken
            or SyntaxKind.GreaterThanOrEqualsToken
            or SyntaxKind.PlusToken
            or SyntaxKind.MinusToken
            or SyntaxKind.StarToken
            or SyntaxKind.SlashToken
            or SyntaxKind.PercentToken
            or SyntaxKind.CaretToken
            or SyntaxKind.AmpersandToken
            or SyntaxKind.AmpersandAmpersandToken
            or SyntaxKind.BarToken
            or SyntaxKind.BarBarToken
            or SyntaxKind.LessThanLessThanToken
            or SyntaxKind.GreaterThanGreaterThanToken
            or SyntaxKind.AndToken
            or SyntaxKind.OrToken
            or SyntaxKind.PipeToken
            or SyntaxKind.TildeToken
            or SyntaxKind.QuestionQuestionToken
            or SyntaxKind.QuestionQuestionEqualsToken
            or SyntaxKind.PlusEqualsToken
            or SyntaxKind.MinusEqualsToken
            or SyntaxKind.StarEqualsToken
            or SyntaxKind.SlashEqualsToken
            or SyntaxKind.AmpersandEqualsToken
            or SyntaxKind.BarEqualsToken
            or SyntaxKind.CaretEqualsToken
            or SyntaxKind.ArrowToken
            or SyntaxKind.FatArrowToken;
    }

    private static bool IsWord(SyntaxKind kind)
    {
        return kind == SyntaxKind.IdentifierToken
            || kind == SyntaxKind.NumericLiteralToken
            || kind == SyntaxKind.StringLiteralToken
            || kind == SyntaxKind.MultiLineStringLiteralToken
            || kind == SyntaxKind.CharacterLiteralToken
            || SyntaxFacts.IsKeywordKind(kind);
    }

    private bool IsUnaryOperatorToken(SyntaxToken token)
    {
        if (!SyntaxFacts.IsUnaryOperatorToken(token.Kind) && token.Kind != SyntaxKind.NotKeyword)
        {
            return false;
        }

        return token.Parent?.Kind is SyntaxKind.UnaryExpression
            or SyntaxKind.PostfixUnaryExpression
            or SyntaxKind.PreIncrementExpression
            or SyntaxKind.PreDecrementExpression
            or SyntaxKind.PostIncrementExpression
            or SyntaxKind.PostDecrementExpression
            or SyntaxKind.UnaryPattern;
    }

    private static bool IsTypeArgumentDelimiter(SyntaxToken token)
    {
        return token.Parent?.Kind is SyntaxKind.TypeArgumentList
            or SyntaxKind.TypeParameterList
            or SyntaxKind.FunctionTypeParameterList;
    }
}
