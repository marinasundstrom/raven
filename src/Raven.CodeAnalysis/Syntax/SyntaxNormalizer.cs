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
    private bool _useFormatterAnnotation;
    private bool _hasFormatterAnnotation;

    public SyntaxNormalizer(int indentSize = 4)
    {
        _indent = new string(' ', indentSize);
    }

    public TSyntax Visit<TSyntax>(TSyntax syntax)
        where TSyntax : SyntaxNode
    {
        return Format(syntax, useFormatterAnnotation: false);
    }

    public TSyntax Format<TSyntax>(TSyntax syntax, bool useFormatterAnnotation)
        where TSyntax : SyntaxNode
    {
        ResetState();
        _useFormatterAnnotation = useFormatterAnnotation;
        _hasFormatterAnnotation = useFormatterAnnotation && HasFormatterAnnotation(syntax);

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

        if (!ShouldRewriteToken(token))
        {
            TrackTokenFlow(token);
            return token;
        }

        if (HasMeaningfulTrivia(token.LeadingTrivia) || HasMeaningfulTrivia(token.TrailingTrivia))
        {
            TrackTokenFlow(token);
            return token;
        }

        var effectiveIndent = _indentLevel;
        if (token.Kind == SyntaxKind.CloseBraceToken && effectiveIndent > 0)
        {
            effectiveIndent--;
        }

        var leading = CreateLeadingTrivia(token, effectiveIndent);
        var trailing = SyntaxFactory.TriviaList();

        token = token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing);

        TrackTokenFlow(token);

        return token;
    }

    private void TrackTokenFlow(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.CloseBraceToken && _indentLevel > 0)
        {
            _indentLevel--;
        }

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
        else if (ShouldStartNewLineBefore(token))
        {
            lineBreaks = Math.Max(lineBreaks, 1);
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

    private static bool HasFormatterAnnotation(SyntaxNode syntax)
    {
        if (syntax.GetAnnotation(Formatter.Annotation.Kind) is not null)
        {
            return true;
        }

        return syntax.DescendantNodes().Any(node => node.GetAnnotation(Formatter.Annotation.Kind) is not null);
    }

    private bool ShouldRewriteToken(SyntaxToken token)
    {
        if (!_useFormatterAnnotation || !_hasFormatterAnnotation)
        {
            return true;
        }

        if (TokenOrAncestorsHaveFormatterAnnotation(token))
        {
            return true;
        }

        return ContainsElasticTrivia(token.LeadingTrivia) || ContainsElasticTrivia(token.TrailingTrivia);
    }

    private static bool TokenOrAncestorsHaveFormatterAnnotation(SyntaxToken token)
    {
        if (token.GetAnnotation(Formatter.Annotation.Kind) is not null)
        {
            return true;
        }

        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (current.GetAnnotation(Formatter.Annotation.Kind) is not null)
            {
                return true;
            }
        }

        return false;
    }

    private static bool ContainsElasticTrivia(SyntaxTriviaList triviaList)
    {
        foreach (var trivia in triviaList)
        {
            if (trivia.IsElastic)
            {
                return true;
            }
        }

        return false;
    }

    private static bool HasMeaningfulTrivia(SyntaxTriviaList triviaList)
    {
        foreach (var trivia in triviaList)
        {
            if (trivia.IsElastic)
            {
                continue;
            }

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

    private bool ShouldStartNewLineBefore(SyntaxToken token)
    {
        if (TryGetEnclosingAccessor(_previousToken, out var previousAccessor) &&
            TryGetEnclosingAccessor(token, out var currentAccessor) &&
            !ReferenceEquals(previousAccessor, currentAccessor) &&
            ReferenceEquals(previousAccessor.Parent, currentAccessor.Parent))
        {
            return true;
        }

        if (TryGetEnclosingBlockStatement(_previousToken, out var previousStatement, out var previousBlock) &&
            TryGetEnclosingBlockStatement(token, out var currentStatement, out var currentBlock) &&
            !ReferenceEquals(previousStatement, currentStatement) &&
            ReferenceEquals(previousBlock, currentBlock))
        {
            return true;
        }

        return false;
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
        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (current.GetLastToken(includeZeroWidth: false) != token)
                continue;

            if (current is AccessorDeclarationSyntax)
                return true;

            if (current is StatementSyntax statement && statement.Parent is BlockStatementSyntax)
                return true;

            if (current.Kind is SyntaxKind.ImportDirective
                or SyntaxKind.AliasDirective
                or SyntaxKind.GlobalStatement
                or SyntaxKind.EnumMemberDeclaration
                or SyntaxKind.UnionCaseClause)
            {
                return true;
            }
        }

        return false;
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

        if (previous.Kind == SyntaxKind.SemicolonToken)
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

    private static bool TryGetEnclosingAccessor(SyntaxToken token, out AccessorDeclarationSyntax accessor)
    {
        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (current is AccessorDeclarationSyntax found)
            {
                accessor = found;
                return true;
            }
        }

        accessor = null!;
        return false;
    }

    private static bool TryGetEnclosingBlockStatement(
        SyntaxToken token,
        out StatementSyntax statement,
        out BlockStatementSyntax block)
    {
        StatementSyntax? foundStatement = null;

        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (current is StatementSyntax currentStatement)
                foundStatement ??= currentStatement;

            if (current is BlockStatementSyntax currentBlock && foundStatement is not null)
            {
                statement = foundStatement;
                block = currentBlock;
                return true;
            }
        }

        statement = null!;
        block = null!;
        return false;
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

        return token.Parent?.Kind is SyntaxKind.PrefixOperatorExpression
            or SyntaxKind.PostfixOperatorExpression
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
