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

        if (!ShouldRewriteToken(token))
        {
            TrackTokenFlow(token, token);
            return token;
        }

        if (HasMeaningfulTrivia(token.LeadingTrivia) || HasMeaningfulTrivia(token.TrailingTrivia))
        {
            TrackTokenFlow(token, token);
            return token;
        }

        var originalToken = token;
        var effectiveIndent = _indentLevel;
        if (token.Kind == SyntaxKind.CloseBraceToken && effectiveIndent > 0)
        {
            effectiveIndent--;
        }

        var leading = CreateLeadingTrivia(token, effectiveIndent);
        var trailing = SyntaxFactory.TriviaList();

        token = token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing);

        TrackTokenFlow(originalToken, token);

        return token;
    }

    private void TrackTokenFlow(SyntaxToken originalToken, SyntaxToken rewrittenToken)
    {
        if (rewrittenToken.Kind == SyntaxKind.CloseBraceToken && _indentLevel > 0)
        {
            _indentLevel--;
        }

        if (rewrittenToken.Kind == SyntaxKind.OpenBraceToken)
        {
            _indentLevel++;
        }

        _pendingLineBreaks = CountLineBreakTrivia(rewrittenToken.TrailingTrivia) > 0
            ? 0
            : GetPendingLineBreaksAfter(originalToken);
        _previousToken = rewrittenToken;
        _isFirstToken = false;
    }

    private SyntaxTriviaList CreateLeadingTrivia(SyntaxToken token, int effectiveIndent)
    {
        if (_isFirstToken)
        {
            return SyntaxFactory.TriviaList();
        }

        if (token.Kind == SyntaxKind.EndOfFileToken)
        {
            return SyntaxFactory.TriviaList();
        }

        var previousHasTrailingLineBreak = CountLineBreakTrivia(_previousToken.TrailingTrivia) > 0;
        var lineBreaks = _pendingLineBreaks;
        if (!previousHasTrailingLineBreak && HasStructuralLeadingLineBreak(token))
        {
            lineBreaks = Math.Max(lineBreaks, 1);
        }

        if (ShouldAttachToPreviousToken(token))
        {
            lineBreaks = 0;
        }
        else if (!previousHasTrailingLineBreak && ShouldStartNewLineBefore(token))
        {
            lineBreaks = Math.Max(lineBreaks, 1);
        }
        else if (token.Kind == SyntaxKind.CloseBraceToken
            && _previousToken.Kind != SyntaxKind.OpenBraceToken
            && !previousHasTrailingLineBreak)
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

        if (previousHasTrailingLineBreak && token.Kind != SyntaxKind.EndOfFileToken)
        {
            var indentText = string.Concat(Enumerable.Repeat(_indent, effectiveIndent));
            return indentText.Length > 0
                ? SyntaxFactory.TriviaList(SyntaxFactory.Whitespace(indentText))
                : SyntaxFactory.TriviaList();
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

    private static bool IsLineBreakTrivia(SyntaxKind kind)
    {
        return kind is SyntaxKind.LineFeedTrivia
            or SyntaxKind.CarriageReturnTrivia
            or SyntaxKind.CarriageReturnLineFeedTrivia
            or SyntaxKind.EndOfLineTrivia;
    }

    private bool ShouldAttachToPreviousToken(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.CloseParenToken && ShouldKeepCloseParenWithBlockBodiedLambda(token))
            return true;

        if (_previousToken.Kind == SyntaxKind.CloseBraceToken)
        {
            if (token.Kind is SyntaxKind.ElseKeyword or SyntaxKind.CatchKeyword or SyntaxKind.FinallyKeyword)
                return true;
        }

        return false;
    }

    private bool ShouldStartNewLineBefore(SyntaxToken token)
    {
        if (TryGetEnclosingLeadingAttributeList(_previousToken, out var previousAttributeList, out var previousAttributeOwner) &&
            TryGetEnclosingLeadingAttributeList(token, out var currentAttributeList, out var currentAttributeOwner) &&
            !ReferenceEquals(previousAttributeList, currentAttributeList) &&
            ReferenceEquals(previousAttributeOwner, currentAttributeOwner))
        {
            return true;
        }

        if (TryGetEnclosingLeadingAttributeList(_previousToken, out _, out var attributeOwnerBefore) &&
            IsFirstTokenAfterLeadingAttributeLists(token, attributeOwnerBefore))
        {
            return true;
        }

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

        if (TryGetEnclosingMemberDeclaration(_previousToken, out var previousMember) &&
            TryGetEnclosingMemberDeclaration(token, out var currentMember) &&
            !ReferenceEquals(previousMember, currentMember) &&
            ReferenceEquals(previousMember.Parent, currentMember.Parent))
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

        if (CountLineBreakTrivia(token.TrailingTrivia) > 0)
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
            var lastToken = current.GetLastToken(includeZeroWidth: false);
            if (!AreSameToken(lastToken, token))
                continue;

            if (current is AccessorDeclarationSyntax)
                return true;

            if (current is StatementSyntax statement && statement.Parent is BlockStatementSyntax)
                return true;

            if (current is MemberDeclarationSyntax member &&
                member.Parent is TypeDeclarationSyntax or CompilationUnitSyntax or BaseNamespaceDeclarationSyntax)
            {
                return true;
            }

            if (current.Kind is SyntaxKind.ImportDirective
                or SyntaxKind.AliasDirective
                or SyntaxKind.GlobalStatement
                or SyntaxKind.MatchArm
                or SyntaxKind.EnumMemberDeclaration
                or SyntaxKind.CaseDeclaration)
            {
                return true;
            }
        }

        return false;
    }

    private static bool AreSameToken(SyntaxToken left, SyntaxToken right)
    {
        if (left.Kind != right.Kind || left.IsMissing != right.IsMissing)
        {
            return false;
        }

        return left.Span.Equals(right.Span);
    }

    private static int CountLineBreakTrivia(SyntaxTriviaList triviaList)
    {
        var count = 0;

        foreach (var trivia in triviaList)
        {
            if (IsLineBreakTrivia(trivia.Kind))
            {
                count++;
            }
        }

        return count;
    }

    private static bool HasStructuralLeadingLineBreak(SyntaxToken token)
    {
        if (CountLineBreakTrivia(token.LeadingTrivia) == 0)
        {
            return false;
        }

        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            var firstToken = current.GetFirstToken(includeZeroWidth: false);
            if (!AreSameToken(firstToken, token))
                continue;

            if (current is AccessorDeclarationSyntax)
                return true;

            if (current is StatementSyntax statement && statement.Parent is BlockStatementSyntax)
                return true;

            if (current is MemberDeclarationSyntax member &&
                member.Parent is TypeDeclarationSyntax or CompilationUnitSyntax or BaseNamespaceDeclarationSyntax)
            {
                return true;
            }

            if (current.Kind is SyntaxKind.ImportDirective
                or SyntaxKind.AliasDirective
                or SyntaxKind.GlobalStatement
                or SyntaxKind.MatchArm
                or SyntaxKind.EnumMemberDeclaration
                or SyntaxKind.CaseDeclaration)
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
                or SyntaxKind.MatchKeyword
                or SyntaxKind.ValKeyword
                or SyntaxKind.LetKeyword
                or SyntaxKind.VarKeyword;
        }

        if (current.Kind == SyntaxKind.OpenBracketToken)
        {
            return previous.Kind is SyntaxKind.ValKeyword
                or SyntaxKind.LetKeyword
                or SyntaxKind.VarKeyword;
        }

        if (current.Kind == SyntaxKind.InKeyword)
        {
            return true;
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

    private static bool TryGetEnclosingMemberDeclaration(SyntaxToken token, out MemberDeclarationSyntax member)
    {
        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (current is MemberDeclarationSyntax found)
            {
                member = found;
                return true;
            }
        }

        member = null!;
        return false;
    }

    private static bool TryGetEnclosingLeadingAttributeList(
        SyntaxToken token,
        [NotNullWhen(true)] out AttributeListSyntax? attributeList,
        [NotNullWhen(true)] out SyntaxNode? owner)
    {
        attributeList = null;
        owner = null;

        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (attributeList is null && current is AttributeListSyntax foundAttributeList)
            {
                attributeList = foundAttributeList;
                continue;
            }

            if (attributeList is null)
                continue;

            if (OwnsLeadingAttributeList(current, attributeList))
            {
                owner = current;
                return true;
            }
        }

        attributeList = null;
        owner = null;
        return false;
    }

    private static bool OwnsLeadingAttributeList(SyntaxNode node, AttributeListSyntax attributeList)
    {
        return node switch
        {
            CompilationUnitSyntax compilationUnit => compilationUnit.AttributeLists.Contains(attributeList),
            MemberDeclarationSyntax member => member.AttributeLists.Contains(attributeList),
            AccessorDeclarationSyntax accessor => accessor.AttributeLists.Contains(attributeList),
            _ => false
        };
    }

    private static bool IsFirstTokenAfterLeadingAttributeLists(SyntaxToken token, SyntaxNode owner)
    {
        foreach (var child in owner.ChildNodesAndTokens())
        {
            if (child.TryGetNode(out var node))
            {
                if (node is AttributeListSyntax)
                    continue;

                return node.GetFirstToken(includeZeroWidth: false) == token;
            }

            if (child.TryGetToken(out var childToken))
            {
                if (childToken.FullWidth == 0)
                    continue;

                return childToken == token;
            }
        }

        return false;
    }

    private static bool ShouldKeepCloseParenWithBlockBodiedLambda(SyntaxToken token)
    {
        return token.Parent is ArgumentListSyntax argumentList
            && argumentList.Arguments.Count > 0
            && argumentList.Arguments.Last().Expression is FunctionExpressionSyntax functionExpression
            && (functionExpression.Body is not null
                || functionExpression.ExpressionBody?.Expression is BlockSyntax);
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
            or SyntaxKind.SuppressNullableWarningExpression
            or SyntaxKind.UnaryPattern;
    }

    private static bool IsTypeArgumentDelimiter(SyntaxToken token)
    {
        return token.Parent?.Kind is SyntaxKind.TypeArgumentList
            or SyntaxKind.TypeParameterList
            or SyntaxKind.FunctionTypeParameterList;
    }
}
