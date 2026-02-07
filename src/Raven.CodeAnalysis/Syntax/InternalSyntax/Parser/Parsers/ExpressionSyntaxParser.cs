namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

using SyntaxFacts = Raven.CodeAnalysis.Syntax.SyntaxFacts;

internal partial class ExpressionSyntaxParser : SyntaxParser
{
    private readonly bool _allowMatchExpressionSuffixes;
    private readonly bool _stopOnOpenBrace;
    private readonly bool _allowLambdaExpressions;
    private const int RangeOperatorPrecedence = 4;

    public ExpressionSyntaxParser(
        ParseContext parent,
        bool allowMatchExpressionSuffixes = true,
        bool stopOnOpenBrace = false,
        bool allowLambdaExpressions = true)
        : base(parent)
    {
        _allowMatchExpressionSuffixes = allowMatchExpressionSuffixes;
        _stopOnOpenBrace = stopOnOpenBrace;
        _allowLambdaExpressions = allowLambdaExpressions;
    }

    public ExpressionSyntaxParser ParentExpression => (ExpressionSyntaxParser)Parent!;

    public ExpressionSyntax ParseExpression()
    {
        return ParseNullCoalesceExpression() ?? new ExpressionSyntax.Missing();
    }

    public ExpressionSyntax ParseExpressionOrNull()
    {
        return ParseNullCoalesceExpression();
    }

    public BlockSyntax ParseBlockSyntax()
    {
        var openBrace = ExpectToken(SyntaxKind.OpenBraceToken);

        EnterParens(); // Treat block as a nesting construct
        var statements = new List<StatementSyntax>();

        while (!IsNextToken(SyntaxKind.CloseBraceToken, out _) &&
               !IsNextToken(SyntaxKind.EndOfFileToken, out _))
        {
            var statementStart = Position;
            var stmt = new StatementSyntaxParser(this).ParseStatement();
            if (stmt is not null)
                statements.Add(stmt);

            if (Position == statementStart)
            {
                var token = PeekToken();
                var tokenText = string.IsNullOrEmpty(token.Text)
                    ? token.Kind.ToString()
                    : token.Text;

                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                        GetSpanOfPeekedToken(),
                        tokenText));

                ReadToken();
            }

            SetTreatNewlinesAsTokens(false);
        }
        ExitParens();

        var closeBrace = ExpectToken(SyntaxKind.CloseBraceToken);

        return Block(openBrace, List(statements), closeBrace);
    }

    private SyntaxList ParseStatementsList(SyntaxKind untilToken, out SyntaxToken token)
    {
        List<StatementSyntax> statements = new List<StatementSyntax>();
        while (!ConsumeToken(untilToken, out token))
        {
            if (PeekToken().IsKind(SyntaxKind.EndOfFileToken))
                break;

            var statementStart = Position;
            var statement = new StatementSyntaxParser(this).ParseStatement();
            statements.Add(statement);

            if (Position == statementStart)
            {
                var current = PeekToken();
                var tokenText = string.IsNullOrEmpty(current.Text)
                    ? current.Kind.ToString()
                    : current.Text;

                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                        GetSpanOfPeekedToken(),
                        tokenText));

                ReadToken();
            }
        }

        return new SyntaxList(statements.ToArray());
    }

    private ExpressionSyntax ParseNullCoalesceExpression()
    {
        // `??` has lower precedence than `||` / `&&` and is right-associative.
        ExpressionSyntax left = ParseOrExpression();

        if (ConsumeToken(SyntaxKind.QuestionQuestionToken, out var operatorToken))
        {
            // Right-associative: a ?? b ?? c == a ?? (b ?? c)
            var right = ParseNullCoalesceExpression();
            return NullCoalesceExpression(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax ParseOrExpression()
    {
        ExpressionSyntax ret = ParseAndExpression();
        SyntaxToken token;
        while (ConsumeToken(SyntaxKind.BarBarToken, out token))
        {
            ret = BinaryExpression(SyntaxKind.LogicalOrExpression, ret, token, ParseAndExpression());
        }
        return ret;
    }

    private SyntaxKind GetBinaryExpressionKind(SyntaxToken operatorToken)
    {
        switch (operatorToken.Kind)
        {
            case SyntaxKind.PlusToken:
                return SyntaxKind.AddExpression;

            case SyntaxKind.MinusToken:
                return SyntaxKind.SubtractExpression;

            case SyntaxKind.StarToken:
                return SyntaxKind.MultiplyExpression;

            case SyntaxKind.SlashToken:
                return SyntaxKind.DivideExpression;

            case SyntaxKind.PercentToken:
                return SyntaxKind.ModuloExpression;

            case SyntaxKind.AmpersandToken:
                return SyntaxKind.BitwiseAndExpression;

            case SyntaxKind.BarToken:
                return SyntaxKind.BitwiseOrExpression;

            case SyntaxKind.PipeToken:
                return SyntaxKind.PipeExpression;

            case SyntaxKind.EqualsEqualsToken:
                return SyntaxKind.EqualsExpression;

            case SyntaxKind.NotEqualsToken:
                return SyntaxKind.NotEqualsExpression;

            case SyntaxKind.LessThanToken:
                return SyntaxKind.LessThanExpression;

            case SyntaxKind.GreaterThanToken:
                return SyntaxKind.GreaterThanExpression;

            case SyntaxKind.LessThanOrEqualsToken:
                return SyntaxKind.LessThanOrEqualsExpression;

            case SyntaxKind.GreaterThanOrEqualsToken:
                return SyntaxKind.GreaterThanOrEqualsExpression;

            case SyntaxKind.AmpersandAmpersandToken:
                return SyntaxKind.LogicalAndExpression;

            case SyntaxKind.BarBarToken:
                return SyntaxKind.LogicalOrExpression;
        }

        throw new ArgumentException("Kind is not valid for this expression.");
    }

    private ExpressionSyntax ParseAndExpression()
    {
        ExpressionSyntax ret = ParseLogicalNotExpression();
        SyntaxToken token;
        while (ConsumeToken(SyntaxKind.AmpersandAmpersandToken, out token))
        {
            ret = BinaryExpression(SyntaxKind.LogicalAndExpression, ret, token, ParseAndExpression());
        }
        return ret;
    }

    private ExpressionSyntax ParseLogicalNotExpression()
    {
        if (ConsumeToken(SyntaxKind.ExclamationToken, out var token))
        {
            ExpressionSyntax ret = UnaryExpression(SyntaxKind.LogicalNotExpression, token, ParseLogicalNotExpression());
            return ret;
        }
        else
        {
            return ParseComparisonExpression();
        }
    }

    /// <summary>
    /// Parse a comparison expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private ExpressionSyntax ParseComparisonExpression()
    {
        ExpressionSyntax expr = ParseExpressionCore(0);
        while (true)
        {
            var token = PeekToken();

            if (_stopOnOpenBrace && token.IsKind(SyntaxKind.OpenBraceToken))
                return expr;

            switch (token.Kind)
            {
                case SyntaxKind.GreaterThanToken:
                case SyntaxKind.LessThanToken:
                case SyntaxKind.GreaterThanOrEqualsToken:
                case SyntaxKind.LessThanOrEqualsToken:
                case SyntaxKind.EqualsEqualsToken:
                case SyntaxKind.NotEqualsToken:
                    ReadToken();
                    break;

                case SyntaxKind.IsKeyword:
                    {
                        ReadToken();
                        var pattern = new PatternSyntaxParser(this).ParsePattern();
                        return IsPatternExpression(expr, token, pattern);
                    }

                case SyntaxKind.AsKeyword:
                    {
                        ReadToken();
                        var type = new NameSyntaxParser(this).ParseTypeName();
                        return AsExpression(expr, token, type);
                    }

                default:
                    return expr;
            }
            ExpressionSyntax rhs = ParseComparisonExpression();
            expr = BinaryExpression(GetBinaryExpressionKind(token), expr, token, rhs);
        }
    }

    /// <summary>
    /// Parse an ExpressionSyntax (Internal)
    /// </summary>
    /// <returns>An expression.</returns>
    /// <param name="precedence">The current level of precedence.</param>
    private ExpressionSyntax ParseExpressionCore(int precedence)
    {
        int start = this.Position;

        PatternSyntax? assignmentPattern = null;
        ExpressionSyntax? expr = null;

        if (precedence == 0 && PeekToken(1).Kind == SyntaxKind.EqualsToken && TryParseAssignmentPattern(out var pattern))
        {
            assignmentPattern = pattern;
        }
        else
        {
            if (PeekToken().IsKind(SyntaxKind.DotDotToken) && precedence <= RangeOperatorPrecedence)
            {
                expr = null;
            }
            else
            {
                expr = ParseFactorExpression();
            }
        }

        if (TryConsumeAssignmentOperator(out var assignToken))
        {
            ExpressionOrPatternSyntax leftNode;
            if (assignmentPattern is not null)
            {
                leftNode = assignmentPattern;
            }
            else
            {
                leftNode = expr!;

                if (expr is not IdentifierNameSyntax
                    and not MemberAccessExpressionSyntax
                    and not MemberBindingExpressionSyntax
                    and not ElementAccessExpressionSyntax)
                {
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetActualTextSpan(start, expr)
                        ));
                }
            }

            var right = ParseExpressionCore(0);

            return AssignmentExpression(GetAssignmentExpressionKind(assignToken), leftNode, assignToken, right, Diagnostics);
        }

        while (true)
        {
            var operatorCandidate = PeekToken();

            if (operatorCandidate.IsKind(SyntaxKind.EndOfFileToken))
                return expr ?? new ExpressionSyntax.Missing();

            if (operatorCandidate.IsKind(SyntaxKind.DotDotToken))
            {
                if (RangeOperatorPrecedence < precedence)
                    return expr ?? new ExpressionSyntax.Missing();

                ReadToken();
                var right = ParseRangeBoundaryExpression();
                expr = RangeExpression(expr, operatorCandidate, right);
                continue;
            }

            int prec;
            if (!TryResolveOperatorPrecedence(operatorCandidate, out prec))
                return expr ?? new ExpressionSyntax.Missing();

            if (prec >= precedence)
            {
                ReadToken();
                var right = ParseExpressionCore(prec + 1);
                expr = BinaryExpression(GetBinaryExpressionKind(operatorCandidate), expr!, operatorCandidate, right);
            }
            else
            {
                return expr ?? new ExpressionSyntax.Missing();
            }
        }
    }

    private ExpressionSyntax? ParseRangeBoundaryExpression()
    {
        var next = PeekToken();

        if (next.IsKind(SyntaxKind.CloseBracketToken) ||
            next.IsKind(SyntaxKind.CloseParenToken) ||
            next.IsKind(SyntaxKind.CloseBraceToken) ||
            next.IsKind(SyntaxKind.CommaToken) ||
            next.IsKind(SyntaxKind.EndOfFileToken))
        {
            return null;
        }

        return ParseExpressionCore(RangeOperatorPrecedence + 1);
    }

    private bool TryParseAssignmentPattern(out PatternSyntax pattern)
    {
        pattern = null!;

        if (!IsPossibleAssignmentPatternStart(PeekToken()))
            return false;

        var checkpoint = CreateCheckpoint("assignment-pattern");

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken) &&
            ContainsAssignmentBeforeMatchingCloseParen())
        {
            checkpoint.Rewind();
            return false;
        }

        var parsedPattern = new PatternSyntaxParser(this).ParsePattern();

        if (!PeekToken().IsKind(SyntaxKind.EqualsToken))
        {
            checkpoint.Rewind();
            return false;
        }

        pattern = parsedPattern;
        return true;
    }

    private bool ContainsAssignmentBeforeMatchingCloseParen()
    {
        var depth = 0;
        var offset = 0;

        while (true)
        {
            var token = PeekToken(offset++);

            if (token.Kind == SyntaxKind.EndOfFileToken)
                return false;

            if (token.Kind == SyntaxKind.OpenParenToken)
            {
                depth++;
                continue;
            }

            if (token.Kind == SyntaxKind.CloseParenToken)
            {
                depth--;

                if (depth == 0)
                    return false;

                continue;
            }

            if (token.Kind == SyntaxKind.EqualsToken && depth > 0)
                return true;
        }
    }

    private static SyntaxKind GetAssignmentExpressionKind(SyntaxToken operatorToken)
    {
        return operatorToken.Kind switch
        {
            SyntaxKind.PlusEqualsToken => SyntaxKind.AddAssignmentExpression,
            SyntaxKind.MinusEqualsToken => SyntaxKind.SubtractAssignmentExpression,
            SyntaxKind.StarEqualsToken => SyntaxKind.MultiplyAssignmentExpression,
            SyntaxKind.SlashEqualsToken => SyntaxKind.DivideAssignmentExpression,
            SyntaxKind.AmpersandEqualsToken => SyntaxKind.BitwiseAndAssignmentExpression,
            SyntaxKind.BarEqualsToken => SyntaxKind.BitwiseOrAssignmentExpression,
            _ => SyntaxKind.SimpleAssignmentExpression,
        };
    }

    private bool TryConsumeAssignmentOperator(out SyntaxToken token)
    {
        if (IsAssignmentOperator(PeekToken().Kind))
        {
            token = ReadToken();
            return true;
        }

        token = null!;
        return false;
    }

    private static bool IsAssignmentOperator(SyntaxKind kind)
    {
        return kind is SyntaxKind.EqualsToken
            or SyntaxKind.PlusEqualsToken
            or SyntaxKind.MinusEqualsToken
            or SyntaxKind.StarEqualsToken
            or SyntaxKind.SlashEqualsToken
            or SyntaxKind.AmpersandEqualsToken
            or SyntaxKind.BarEqualsToken;
    }

    private static bool IsPossibleAssignmentPatternStart(SyntaxToken token)
    {
        return token.Kind switch
        {
            SyntaxKind.OpenParenToken => true,
            SyntaxKind.LetKeyword => true,
            SyntaxKind.ValKeyword => true,
            SyntaxKind.VarKeyword => true,
            SyntaxKind.UnderscoreToken => true,
            _ => false,
        };
    }

    /// <summary>
    /// Try to resolve an operation from a specified candidate token.
    /// </summary>
    /// <returns><c>true</c>, if the token is an operation, <c>false</c> otherwise.</returns>
    /// <param name="candidateToken">The candidate token for operation.</param>
    /// <param name="precedence">The operator precedence for the resolved operation.</param>
    private bool TryResolveOperatorPrecedence(SyntaxToken candidateToken, out int precedence)
    {
        return SyntaxFacts.TryResolveOperatorPrecedence(candidateToken.Kind, out precedence);
    }

    /// <summary>
    /// Parse a factor expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private ExpressionSyntax ParseFactorExpression()
    {
        if (_allowLambdaExpressions && TryParseLambdaExpression(out var lambda))
        {
            if (!_allowMatchExpressionSuffixes)
                return lambda;

            return ParseMatchExpressionSuffixes(lambda);
        }

        ExpressionSyntax expr;

        SyntaxToken token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.PlusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.UnaryPlusExpression, token, expr);
                break;

            case SyntaxKind.PlusPlusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.PreIncrementExpression, token, expr);
                break;

            case SyntaxKind.MinusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.UnaryMinusExpression, token, expr);
                break;

            case SyntaxKind.MinusMinusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.PreDecrementExpression, token, expr);
                break;

            case SyntaxKind.AmpersandToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.AddressOfExpression, token, expr);
                break;

            case SyntaxKind.CaretToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = IndexExpression(token, expr);
                break;

            case SyntaxKind.IfKeyword:
                expr = ParseIfExpressionSyntax();
                break;

            case SyntaxKind.TryKeyword:
                expr = ParseTryExpression();
                break;

            case SyntaxKind.AwaitKeyword:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.AwaitExpression, token, expr);
                break;

            case SyntaxKind.OpenBraceToken:
                expr = ParseBlockSyntax();
                break;

            default:
                expr = ParsePowerExpression();
                break;
        }

        if (!_allowMatchExpressionSuffixes)
            return expr;

        return ParseMatchExpressionSuffixes(expr);
    }

    private TryExpressionSyntax ParseTryExpression()
    {
        var tryKeyword = ReadToken();
        var questionToken = ConsumeToken(SyntaxKind.QuestionToken, out var consumedQuestionToken)
            ? consumedQuestionToken
            : Token(SyntaxKind.None);
        var expression = new ExpressionSyntaxParser(this, allowMatchExpressionSuffixes: false).ParseExpression();
        return TryExpression(tryKeyword, questionToken, expression);
    }

    private bool TryParseLambdaExpression(out LambdaExpressionSyntax? lambda)
    {
        lambda = null;

        var token = PeekToken();

        // Lambda fast-path: if there's no `=>` ahead before newline/terminator, don't even try.
        // This avoids the checkpoint/rewind churn shown in the trace.
        if (!LooksLikeLambdaAhead(startOffset: 0))
            return false;

        if (token.Kind == SyntaxKind.AsyncKeyword)
        {
            var checkpoint = CreateCheckpoint("async-lambda");
            var asyncKeyword = ReadToken();

            if (PeekToken().Kind == SyntaxKind.OpenParenToken)
            {
                if (!IsParenthesizedCastAhead() && TryParseParenthesizedLambdaExpression(asyncKeyword, out lambda))
                    return true;
            }
            else if (CanTokenBeIdentifier(PeekToken()) && TryParseSimpleLambdaExpression(asyncKeyword, out lambda))
            {
                return true;
            }

            checkpoint.Rewind();
            return false;
        }

        if (token.Kind == SyntaxKind.OpenParenToken)
        {
            if (IsParenthesizedCastAhead())
                return false;

            if (TryParseParenthesizedLambdaExpression(asyncKeyword: null, out lambda))
                return true;
        }

        if (CanTokenBeIdentifier(token) && TryParseSimpleLambdaExpression(asyncKeyword: null, out lambda))
            return true;

        return false;
    }

    private bool IsParenthesizedCastAhead()
    {
        using var checkpoint = CreateCheckpoint("lambda-cast-lookahead");

        if (!PeekToken().IsKind(SyntaxKind.OpenParenToken))
            return false;

        ReadToken();

        var typeName = new NameSyntaxParser(this).ParseTypeName();

        if (typeName.IsMissing)
            return false;

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
            return false;

        ReadToken();

        var next = PeekToken();

        return !next.IsKind(SyntaxKind.FatArrowToken);
    }

    private bool TryParseParenthesizedLambdaExpression(SyntaxToken? asyncKeyword, out LambdaExpressionSyntax? lambda)
    {
        lambda = null;

        if (!PeekToken().IsKind(SyntaxKind.OpenParenToken))
            return false;

        // Fast-path: only speculate if we can see a `=>` ahead before newline/terminator.
        if (!LooksLikeLambdaAhead(startOffset: 0))
            return false;

        var checkpoint = CreateCheckpoint("parenthesized-lambda");

        var parameterList = new StatementSyntaxParser(this).ParseParameterList();

        var returnType = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        if (returnType is null && !IsNextToken(SyntaxKind.FatArrowToken))
        {
            checkpoint.Rewind();
            return false;
        }

        ConsumeTokenOrMissing(SyntaxKind.FatArrowToken, out var fatArrowToken);

        var body = new ExpressionSyntaxParser(this).ParseExpression();

        lambda = ParenthesizedLambdaExpression(
            asyncKeyword,
            parameterList,
            returnType,
            fatArrowToken,
            body);

        return true;
    }

    private bool TryParseSimpleLambdaExpression(SyntaxToken? asyncKeyword, out LambdaExpressionSyntax? lambda)
    {
        lambda = null;

        // Fast-path: don't speculate unless we can see a `=>` before newline/terminator.
        if (!LooksLikeLambdaAhead(startOffset: 0))
            return false;

        var checkpoint = CreateCheckpoint("simple-lambda");

        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

        if (asyncKeyword is null && ConsumeToken(SyntaxKind.AsyncKeyword, out var parsedAsync))
            asyncKeyword = parsedAsync;

        SyntaxToken? refKindKeyword = null;
        if (ConsumeToken(SyntaxKind.RefKeyword, out var modifier)
            || ConsumeToken(SyntaxKind.OutKeyword, out modifier)
            || ConsumeToken(SyntaxKind.InKeyword, out modifier))
        {
            refKindKeyword = modifier;
        }

        SyntaxToken? bindingKeyword = null;
        if (ConsumeToken(SyntaxKind.LetKeyword, out var binding)
            || ConsumeToken(SyntaxKind.ValKeyword, out binding)
            || ConsumeToken(SyntaxKind.VarKeyword, out binding)
            || ConsumeToken(SyntaxKind.ConstKeyword, out binding))
        {
            bindingKeyword = binding;
        }

        if (!CanTokenBeIdentifier(PeekToken()))
        {
            checkpoint.Rewind();
            return false;
        }

        var identifier = ReadIdentifierToken();

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

        EqualsValueClauseSyntax? defaultValue = null;
        if (IsNextToken(SyntaxKind.EqualsToken, out _))
            defaultValue = new EqualsValueClauseSyntaxParser(this).Parse();

        var returnType = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        if (returnType is null && !IsNextToken(SyntaxKind.FatArrowToken))
        {
            checkpoint.Rewind();
            return false;
        }

        ConsumeTokenOrMissing(SyntaxKind.FatArrowToken, out var fatArrowToken);

        var body = new ExpressionSyntaxParser(this).ParseExpression();

        var parameter = Parameter(attributeLists, refKindKeyword, bindingKeyword, identifier, typeAnnotation, defaultValue);

        lambda = SimpleLambdaExpression(asyncKeyword, parameter, returnType, fatArrowToken, body);

        return true;
    }

    /// <summary>
    /// Parse a power expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private ExpressionSyntax ParsePowerExpression()
    {
        var start = Position;

        ExpressionSyntax expr = ParsePrimaryExpression();

        expr = AddTrailers(start, expr);

        if (ConsumeToken(SyntaxKind.CaretToken, out var token))
        {
            ExpressionSyntax right = ParseFactorExpression();
            expr = BinaryExpression(SyntaxKind.PowerExpression, expr, token, right);
        }

        return expr;
    }

    private ExpressionSyntax AddTrailers(int start, ExpressionSyntax expr)
    {
        while (true) // Loop to handle consecutive member access and invocations
        {
            var token = PeekToken();

            // In statement/condition contexts (if/while/for headers, if-expression condition),
            // `{` begins the following block expression/body and must not be consumed as an
            // object-initializer trailer.
            if (_stopOnOpenBrace && token.IsKind(SyntaxKind.OpenBraceToken))
                break;

            if (token.IsKind(SyntaxKind.OpenParenToken)) // Invocation
            {
                // Break if the '(' token has a leading newline.
                // This prevents: <expr> [newline] '('
                if (HasLeadingNewLine(token))
                    return expr;

                var argumentList = ParseArgumentListSyntax();

                // Object initializer may immediately follow an invocation: Foo(...) { ... }
                // But in statement/header contexts (for/if/while conditions), `{` begins the body block,
                // so we must not consume it as an initializer.
                ObjectInitializerExpressionSyntax? initializer = null;
                if (!_stopOnOpenBrace && PeekToken().IsKind(SyntaxKind.OpenBraceToken))
                    initializer = ParseObjectInitializerExpression();

                expr = InvocationExpression(expr, argumentList, initializer);
            }
            else if (token.IsKind(SyntaxKind.DotToken)) // Member Access
            {
                var dotToken = ReadToken();
                SimpleNameSyntax memberName;
                if (CanTokenBeIdentifier(PeekToken()))
                {
                    memberName = new NameSyntaxParser(this).ParseSimpleName();
                }
                else
                {
                    ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier);
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetEndOfLastToken()));
                    memberName = IdentifierName(identifier);
                }
                expr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, dotToken, memberName);
            }
            else if (token.IsKind(SyntaxKind.OpenBracketToken)) // Element access
            {
                // Break if the '[' token has a leading newline.
                // This prevents: <expr> [newline] '['
                if (HasLeadingNewLine(token))
                    return expr;
                var argumentList = ParseBracketedArgumentListSyntax();

                expr = ElementAccessExpression(expr, argumentList, Diagnostics);
            }
            else if (token.IsKind(SyntaxKind.OpenBraceToken)) // Object initializer trailer (SwiftUI/Flutter-style)
            {
                // Treat `<expr> { ... }` as an invocation with a missing argument list plus an initializer.
                // This enables: `Window { ... }` where `Window` is parsed as an IdentifierName.

                var initializer = ParseObjectInitializerExpression();

                if (expr is InvocationExpressionSyntax inv)
                {
                    // If it's already an invocation (e.g. Foo() { ... }), just attach initializer.
                    expr = InvocationExpression(inv.Expression, inv.ArgumentList, initializer);
                }
                else
                {
                    // Synthesize a missing argument list: `expr(/*missing*/){...}`
                    var missingArgs = CreateMissingArgumentList();
                    expr = InvocationExpression(expr, missingArgs, initializer);
                }
            }
            else if (token.IsKind(SyntaxKind.WithKeyword)) // With-expression trailer: `<expr> with { ... }`
            {
                // Break if the `with` token has a leading newline.
                // This prevents: <expr> [newline] with { ... }
                if (HasLeadingNewLine(token))
                    return expr;

                expr = ParseWithExpression(expr);
            }
            else if (token.IsKind(SyntaxKind.QuestionToken)) // Conditional access OR propagate (`<expr>?`)
            {
                var operatorToken = ReadToken();
                var next = PeekToken();

                // Conditional access only when followed by one of the conditional-access trailers.
                // Otherwise, treat `?` as the Result-propagation postfix operator.
                if (!next.IsKind(SyntaxKind.DotToken)
                    && !next.IsKind(SyntaxKind.OpenParenToken)
                    && !next.IsKind(SyntaxKind.OpenBracketToken))
                {
                    expr = PropagateExpression(expr, operatorToken);
                    continue;
                }

                ExpressionSyntax whenNotNull;
                if (next.IsKind(SyntaxKind.DotToken))
                {
                    var dotToken = ReadToken();
                    SimpleNameSyntax memberName;
                    if (CanTokenBeIdentifier(PeekToken()))
                    {
                        memberName = new NameSyntaxParser(this).ParseSimpleName();
                    }
                    else
                    {
                        ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier);
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.IdentifierExpected,
                                GetEndOfLastToken()));
                        memberName = IdentifierName(identifier);
                    }

                    var memberBinding = MemberBindingExpression(dotToken, memberName);
                    if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
                    {
                        var argumentList = ParseArgumentListSyntax();
                        whenNotNull = InvocationExpression(memberBinding, argumentList);
                    }
                    else
                    {
                        whenNotNull = memberBinding;
                    }
                }
                else if (next.IsKind(SyntaxKind.OpenParenToken))
                {
                    var argumentList = ParseArgumentListSyntax();
                    var receiverBinding = ReceiverBindingExpression(Token(SyntaxKind.None));
                    whenNotNull = InvocationExpression(receiverBinding, argumentList);
                }
                else if (next.IsKind(SyntaxKind.OpenBracketToken))
                {
                    var argumentList = ParseBracketedArgumentListSyntax();
                    whenNotNull = ElementBindingExpression(argumentList);
                }
                else
                {
                    ConsumeTokenOrMissing(SyntaxKind.DotToken, out var missingDot);
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetEndOfLastToken()));
                    var missingName = IdentifierName(SyntaxFactory.MissingToken(SyntaxKind.IdentifierToken));
                    whenNotNull = MemberBindingExpression(missingDot, missingName);
                }

                expr = ConditionalAccessExpression(expr, operatorToken, whenNotNull);
            }
            else if (token.IsKind(SyntaxKind.PlusPlusToken)) // Post-increment
            {
                var operatorToken = ReadToken();
                expr = PostfixUnaryExpression(SyntaxKind.PostIncrementExpression, expr, operatorToken);
            }
            else if (token.IsKind(SyntaxKind.MinusMinusToken)) // Post-decrement
            {
                var operatorToken = ReadToken();
                expr = PostfixUnaryExpression(SyntaxKind.PostDecrementExpression, expr, operatorToken);
            }
            else
            {
                // No more trailers, break out of the loop
                break;
            }
        }

        return expr;
    }

    internal ArgumentListSyntax ParseArgumentListSyntax(bool allowLegacyNamedArgumentEquals = true)
    {
        // We assume current token is '('
        var openParenToken = ReadToken();

        var argumentList = new List<GreenNode>();
        var seenNames = new HashSet<string>();
        int parsedArgs = 0;

        // Inside a parenthesized argument list, newlines should behave like trivia.
        // Save & restore whatever backing state you use for this.
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens; // or a property getter if you have one
        SetTreatNewlinesAsTokens(false);

        SyntaxToken closeParenToken;

        try
        {
            while (true)
            {
                var t = PeekToken();

                while (IsNewLineLike(t))
                {
                    ReadToken();
                    t = PeekToken();
                }

                // End of argument list
                if (t.IsKind(SyntaxKind.EndOfFileToken) ||
                    t.IsKind(SyntaxKind.CloseParenToken))
                {
                    break;
                }

                // After the first argument, we expect a comma before the next one.
                if (parsedArgs > 0)
                {
                    t = PeekToken();

                    if (t.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        argumentList.Add(commaToken);
                    }
                    else
                    {
                        // Newlines are trivia here, so if we see *anything* other than a comma
                        // we complain about a missing ',' but still try to parse the next arg.
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ","));
                    }
                }

                // Parse the argument expression itself.
                // Newlines are currently *not* tokens at this level, so examples like:
                //
                //   Foo(
                //      42)
                //
                // and
                //
                //   Foo(a,
                //      42)
                //
                // just see the newlines as trivia around '42'.
                var argumentStart = Position;
                var arg = new ExpressionSyntaxParser(this).ParseArgument(allowLegacyNamedArgumentEquals, out var nameSpan);

                if (arg is null or { IsMissing: true })
                {
                    var peekedToken = PeekToken();
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.InvalidExpressionTerm,
                            GetSpanOfPeekedToken(),
                            peekedToken.Text));
                }
                else if (arg.NameColon is { } nameColon)
                {
                    var name = nameColon.Name.Identifier.GetValueText();
                    if (!seenNames.Add(name))
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.DuplicateNamedArgument,
                                nameSpan ?? GetSpanOfLastToken(),
                                name));
                    }

                    if (!allowLegacyNamedArgumentEquals &&
                        (nameColon.ColonToken.IsMissing || nameColon.ColonToken.Kind != SyntaxKind.ColonToken))
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ":"));
                    }
                }

                if (Position == argumentStart)
                {
                    var token = PeekToken();
                    var tokenText = string.IsNullOrEmpty(token.Text)
                        ? token.Kind.ToString()
                        : token.Text;

                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                            GetSpanOfPeekedToken(),
                            tokenText));

                    if (token.IsKind(SyntaxKind.CloseParenToken) || token.IsKind(SyntaxKind.EndOfFileToken))
                        break;

                    ReadToken();
                    continue;
                }

                argumentList.Add(arg);
                parsedArgs++;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out closeParenToken);
        }
        finally
        {
            // Make sure we put the global newline behavior back
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        if (closeParenToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    ")"));
        }

        return ArgumentList(openParenToken, List(argumentList.ToArray()), closeParenToken, Diagnostics);
    }

    public ArgumentSyntax ParseArgument()
    {
        return ParseArgument(allowLegacyNamedArgumentEquals: true, out _);
    }

    public ArgumentSyntax ParseArgument(out TextSpan? nameSpan)
    {
        return ParseArgument(allowLegacyNamedArgumentEquals: true, out nameSpan);
    }

    public ArgumentSyntax ParseArgument(bool allowLegacyNamedArgumentEquals, out TextSpan? nameSpan)
    {
        NameColonSyntax? nameColon = null;
        nameSpan = null;

        // Try to parse optional name:
        if (PeekToken(1).IsKind(SyntaxKind.ColonToken)
            && CanTokenBeIdentifier(PeekToken()))
        {
            var name = ReadToken(); // identifier or keyword
            if (name.Kind != SyntaxKind.IdentifierToken)
            {
                name = ToIdentifierToken(name);
                UpdateLastToken(name);
            }
            nameSpan = GetSpanOfLastToken();
            var colon = ReadToken(); // colon
            nameColon = NameColon(IdentifierName(name), colon);
        }
        else if (PeekToken(1).IsKind(SyntaxKind.EqualsToken)
           && CanTokenBeIdentifier(PeekToken()))
        {
            var name = ReadToken(); // identifier or keyword
            if (name.Kind != SyntaxKind.IdentifierToken)
            {
                name = ToIdentifierToken(name);
                UpdateLastToken(name);
            }
            nameSpan = GetSpanOfLastToken();
            var equals = ReadToken();

            if (allowLegacyNamedArgumentEquals)
            {
                // Backward compatibility for regular invocation argument lists.
                // Attribute argument lists intentionally reject this form.
                nameColon = NameColon(IdentifierName(name), equals);
            }
            else
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetSpanOfLastToken(),
                        ":"));

                nameColon = NameColon(IdentifierName(name), MissingToken(SyntaxKind.ColonToken));
            }
        }

        var expr = ParseExpression();
        return Argument(nameColon, expr);
    }

    private BracketedArgumentListSyntax ParseBracketedArgumentListSyntax()
    {
        var openBracketToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        var parsedArgs = 0;
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken closeBracketToken;

        try
        {
            while (true)
            {
                var t = PeekToken();

                while (IsNewLineLike(t))
                {
                    ReadToken();
                    t = PeekToken();
                }

                if (t.IsKind(SyntaxKind.EndOfFileToken) ||
                    t.IsKind(SyntaxKind.CloseBracketToken))
                {
                    break;
                }

                if (parsedArgs > 0)
                {
                    if (t.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        argumentList.Add(commaToken);
                    }
                    else
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ","));
                    }
                }

                var argumentStart = Position;
                var argument = new ExpressionSyntaxParser(this).ParseArgument();

                if (argument is null or { IsMissing: true })
                {
                    var peekedToken = PeekToken();
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.InvalidExpressionTerm,
                            GetSpanOfPeekedToken(),
                            peekedToken.Text));
                }

                if (Position == argumentStart)
                {
                    var token = PeekToken();
                    var tokenText = string.IsNullOrEmpty(token.Text)
                        ? token.Kind.ToString()
                        : token.Text;

                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                            GetSpanOfPeekedToken(),
                            tokenText));

                    if (token.IsKind(SyntaxKind.CloseBracketToken) || token.IsKind(SyntaxKind.EndOfFileToken))
                        break;

                    ReadToken();
                    continue;
                }

                argumentList.Add(argument);
                parsedArgs++;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out closeBracketToken);
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        if (closeBracketToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    "]"));
        }

        return BracketedArgumentList(openBracketToken, List(argumentList.ToArray()), closeBracketToken, Diagnostics);
    }

    private static bool IsNewLineLike(SyntaxKind kind)
    {
        return kind is SyntaxKind.NewLineToken
            or SyntaxKind.LineFeedToken
            or SyntaxKind.CarriageReturnToken
            or SyntaxKind.CarriageReturnLineFeedToken
            or SyntaxKind.EndOfLineTrivia;
    }

    private static bool IsNewLineLike(SyntaxToken token)
    {
        return IsNewLineLike(token.Kind);
    }

    private static bool HasLeadingNewLine(SyntaxToken token)
    {
        foreach (var trivia in token.LeadingTrivia)
        {
            if (IsNewLineLike(trivia.Kind))
                return true;
        }

        return false;
    }

    private TupleExpressionSyntax ParseTupleExpressionSyntax()
    {
        var openParenToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken) || t.IsKind(SyntaxKind.EndOfFileToken))
                break;

            var elementStart = Position;
            var expression = new ExpressionSyntaxParser(this).ParseExpression();
            if (expression is null)
                break;

            argumentList.Add(Argument(null, expression));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                argumentList.Add(commaToken);
            }

            if (Position == elementStart)
            {
                var current = PeekToken();
                var tokenText = string.IsNullOrEmpty(current.Text)
                    ? current.Kind.ToString()
                    : current.Text;

                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                        GetSpanOfPeekedToken(),
                        tokenText));

                if (current.IsKind(SyntaxKind.CloseParenToken) || current.IsKind(SyntaxKind.EndOfFileToken))
                    break;

                ReadToken();
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return TupleExpression(openParenToken, List(argumentList.ToArray()), closeParenToken);
    }

    /// <summary>
    /// Parse a primary expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private ExpressionSyntax ParsePrimaryExpression()
    {
        ExpressionSyntax expr = null;

        SyntaxToken token;

        token = PeekToken();

        // Allow expressions to continue on the next line. Previously any token
        // with leading end-of-line trivia was treated as a missing identifier,
        // causing line continuations like `let x =\n    42` to fail with a
        // diagnostic. By removing this check the parser treats the newline as
        // trivia and correctly parses the following expression token.

        switch (token.Kind)
        {
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.IntKeyword:
            case SyntaxKind.DoubleKeyword:
            case SyntaxKind.DecimalKeyword:
            case SyntaxKind.ObjectKeyword:
                return ParsePredefinedTypeSyntax();

            case SyntaxKind.IdentifierToken:
                return new NameSyntaxParser(this).ParseSimpleName();

            case SyntaxKind.SelfKeyword:
                ReadToken();
                return SelfExpression(token);

            case SyntaxKind.TrueKeyword:
                ReadToken();
                expr = LiteralExpression(SyntaxKind.TrueLiteralExpression, token);
                break;

            case SyntaxKind.FalseKeyword:
                ReadToken();
                expr = LiteralExpression(SyntaxKind.FalseLiteralExpression, token);
                break;

            case SyntaxKind.NumericLiteralToken:
                expr = ParseNumericLiteralExpressionSyntax();
                break;

            case SyntaxKind.StringLiteralToken:

                ReadToken();
                var tokenText = token.Text;
                var inner = tokenText.Length >= 2 ? tokenText.Substring(1, tokenText.Length - 2) : string.Empty;
                expr = ContainsInterpolation(inner)
                    ? ParseInterpolatedStringExpression(token)
                    : LiteralExpression(SyntaxKind.StringLiteralExpression, token);
                break;

            case SyntaxKind.MultiLineStringLiteralToken:
                // Use raw token text to preserve correct spans (Value may already be indentation‑trimmed)
                ReadToken();
                var multiInner = token.Text.Length >= 6
                    ? token.Text.Substring(3, token.Text.Length - 6)
                    : string.Empty;

                expr = ContainsInterpolation(multiInner)
                    ? ParseInterpolatedMultiLineStringExpression(token, multiInner)
                    : LiteralExpression(SyntaxKind.StringLiteralExpression, token);
                break;

            case SyntaxKind.CharacterLiteralToken:
                ReadToken();
                expr = LiteralExpression(SyntaxKind.CharacterLiteralExpression, token);
                break;

            case SyntaxKind.NullKeyword:
                ReadToken();
                expr = LiteralExpression(SyntaxKind.NullLiteralExpression, token);
                break;

            case SyntaxKind.UnderscoreToken:
                ReadToken();
                expr = DiscardExpression(token);
                break;

            case SyntaxKind.OpenParenToken:
                expr = ParseParenthesisOrTupleExpression();
                break;

            case SyntaxKind.NewKeyword:
                expr = ParseNewExpression();
                break;

            case SyntaxKind.DefaultKeyword:
                expr = ParseDefaultExpression();
                break;

            case SyntaxKind.TypeOfKeyword:
                expr = ParseTypeOfExpression();
                break;

            case SyntaxKind.NameOfKeyword:
                expr = ParseNameOfExpression();
                break;

            case SyntaxKind.OpenBracketToken:
                expr = ParseCollectionExpression();
                break;

            case SyntaxKind.DotToken:
                {
                    var dot = ReadToken();
                    var name = new NameSyntaxParser(this).ParseSimpleName();
                    expr = MemberBindingExpression(dot, name);
                    break;
                }
        }

        if (expr is null
            && SyntaxFacts.IsKeywordKind(token.Kind)
            && !SyntaxFacts.IsReservedWordKind(token.Kind))
        {
            var identifier = ReadToken();
            expr = IdentifierName(identifier);
        }

        return expr ?? new ExpressionSyntax.Missing();
    }

    private ExpressionSyntax ParseParenthesisOrTupleExpression()
    {
        var openParenToken = ReadToken(); // Consumes '('

        if (PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            var close = ReadToken();
            return UnitExpression(openParenToken, close);
        }

        // Try to parse as a cast expression
        var checkpoint = CreateCheckpoint();
        var typeName = new NameSyntaxParser(this).ParseTypeName();
        if (PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            var closeParen = ReadToken();
            var next = PeekToken();
            if (!next.IsKind(SyntaxKind.CommaToken) &&
                !next.IsKind(SyntaxKind.CloseParenToken) &&
                !next.IsKind(SyntaxKind.SemicolonToken) &&
                !next.IsKind(SyntaxKind.EndOfFileToken) &&
                !next.IsKind(SyntaxKind.OpenBraceToken))
            {
                var expression = ParseFactorExpression();
                return CastExpression(openParenToken, typeName, closeParen, expression);
            }
        }
        checkpoint.Rewind();

        var expressions = new List<GreenNode>();

        var firstExpr = new ExpressionSyntaxParser(this).ParseArgument();
        expressions.Add(firstExpr);

        bool sawComma = false;

        while (PeekToken().IsKind(SyntaxKind.CommaToken))
        {
            sawComma = true;
            expressions.Add(ReadToken()); // Consume comma
            var arg = new ExpressionSyntaxParser(this).ParseArgument();
            expressions.Add(arg);
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        if (sawComma)
        {
            // This was a tuple
            return TupleExpression(openParenToken, List(expressions.ToArray()), closeParenToken);
        }
        else
        {
            // Just a parenthesized expression
            return ParenthesizedExpression(openParenToken, (ExpressionSyntax)firstExpr.GetSlot(1), closeParenToken, Diagnostics);
        }
    }

    private ExpressionSyntax ParseCollectionExpression()
    {
        var openBracketToken = ReadToken();

        List<GreenNode> elementList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBracketToken) || t.IsKind(SyntaxKind.EndOfFileToken))
                break;

            CollectionElementSyntax element;
            var elementStart = Position;

            if (t.IsKind(SyntaxKind.DotDotToken))
            {
                var dotDotToken = ReadToken();
                var spreadExpr = new ExpressionSyntaxParser(this).ParseExpression();
                if (spreadExpr is null)
                    break;
                element = SpreadElement(dotDotToken, spreadExpr);
            }
            else
            {
                var expression = new ExpressionSyntaxParser(this).ParseExpression();
                if (expression is null)
                    break;
                element = ExpressionElement(expression);
            }

            elementList.Add(element);

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                elementList.Add(commaToken);
            }

            if (Position == elementStart)
            {
                var current = PeekToken();
                var tokenText = string.IsNullOrEmpty(current.Text)
                    ? current.Kind.ToString()
                    : current.Text;

                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                        GetSpanOfPeekedToken(),
                        tokenText));

                if (current.IsKind(SyntaxKind.CloseBracketToken) || current.IsKind(SyntaxKind.EndOfFileToken))
                    break;

                ReadToken();
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);

        return CollectionExpression(openBracketToken, List(elementList), closeBracketToken);
    }

    private ExpressionSyntax ParseNewExpression()
    {
        var newKeyword = ReadToken();

        var typeName = new NameSyntaxParser(this).ParseTypeName();

        var args = ParseArgumentListSyntax();

        ObjectInitializerExpressionSyntax? initializer = null;
        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
            initializer = ParseObjectInitializerExpression();

        return ObjectCreationExpression(newKeyword, typeName, args, initializer);
    }

    private ExpressionSyntax ParseDefaultExpression()
    {
        var defaultKeyword = ReadToken();

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            var openParenToken = ReadToken();
            var type = new NameSyntaxParser(this).ParseTypeName();
            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

            return DefaultExpression(defaultKeyword, openParenToken, type, closeParenToken);
        }

        var missingOpenParen = MissingToken(SyntaxKind.OpenParenToken);
        var missingCloseParen = MissingToken(SyntaxKind.CloseParenToken);

        return DefaultExpression(defaultKeyword, missingOpenParen, null, missingCloseParen);
    }

    private ExpressionSyntax ParseTypeOfExpression()
    {
        var typeOfKeyword = ReadToken();

        ConsumeTokenOrMissing(SyntaxKind.OpenParenToken, out var openParenToken);

        var type = new NameSyntaxParser(this).ParseTypeName();

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return TypeOfExpression(typeOfKeyword, openParenToken, type, closeParenToken);
    }

    private ExpressionSyntax ParseNameOfExpression()
    {
        var nameOfKeyword = ReadToken();

        ConsumeTokenOrMissing(SyntaxKind.OpenParenToken, out var openParenToken);

        // `nameof` accepts a restricted operand syntax. We prefer parsing a type name first
        // so `nameof(List<int>)` doesn't get mis-parsed as a comparison expression.
        ExpressionSyntax operand;

        var checkpoint = CreateCheckpoint("nameof-operand");
        var type = new NameSyntaxParser(this).ParseTypeName();

        if (!type.IsMissing && PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            operand = type;
        }
        else
        {
            checkpoint.Rewind();
            operand = ParseNameOfOperandExpression();
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return NameOfExpression(nameOfKeyword, openParenToken, operand, closeParenToken);
    }

    private ExpressionSyntax ParseNameOfOperandExpression()
    {
        // Allowed syntactic forms (validated further by the binder):
        //   Identifier
        //   MemberAccessExpression (a.b)
        //   MemberBindingExpression (.b)
        //   QualifiedName (parsed as member access in expression context)
        //
        // We intentionally do not treat invocations, element access, literals, etc. as valid operands.

        var start = Position;

        ExpressionSyntax expr;

        var token = PeekToken();
        if (token.IsKind(SyntaxKind.DotToken))
        {
            // `.WriteLine`
            var dot = ReadToken();
            var name = new NameSyntaxParser(this).ParseSimpleName();
            expr = MemberBindingExpression(dot, name);
        }
        else
        {
            // Parse a normal expression, but we'll validate it below.
            expr = new ExpressionSyntaxParser(this, allowMatchExpressionSuffixes: false).ParseExpression();
        }

        // If the parsed expression is not a name-like expression, report a diagnostic.
        if (expr is not IdentifierNameSyntax
            && expr is not MemberAccessExpressionSyntax
            && expr is not MemberBindingExpressionSyntax)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.IdentifierExpected,
                    GetActualTextSpan(start, expr)));
        }

        return expr;
    }

    private ExpressionSyntax ParsePredefinedTypeSyntax()
    {
        var token = ReadToken();
        return PredefinedType(token);
    }

    private ExpressionSyntax ParseParenthesisExpression()
    {
        var openParenToken = ReadToken();

        var expr = new ExpressionSyntaxParser(this).ParseExpression();

        if (!ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken))
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                )); ;
        }

        return ParenthesizedExpression(openParenToken, expr, closeParenToken, Diagnostics);
    }

    private ExpressionSyntax ParseNumericLiteralExpressionSyntax()
    {
        var token = ReadToken();
        if (token.IsKind(SyntaxKind.NumericLiteralToken))
        {
            return LiteralExpression(SyntaxKind.NumericLiteralExpression, token);
        }

        throw new Exception();
    }

    private ExpressionSyntax ParseMatchExpressionSuffixes(ExpressionSyntax expression)
    {
        while (PeekToken().IsKind(SyntaxKind.MatchKeyword))
        {
            var disallowTryPropagationMatch = expression is TryExpressionSyntax tryExpression &&
                                             tryExpression.QuestionToken.Kind != SyntaxKind.None;
            expression = ParseMatchExpressionSuffix(expression, disallowTryPropagationMatch);
        }

        return expression;
    }

    private MatchExpressionSyntax ParseMatchExpressionSuffix(ExpressionSyntax scrutinee, bool disallowTryPropagationMatch)
    {
        var matchKeyword = ReadToken();

        if (disallowTryPropagationMatch)
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.TryPropagationCannotUseMatch,
                GetSpanOfLastToken()));
        }

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        var arms = new List<MatchArmSyntax>();

        var previousTreatNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        EnterParens();
        try
        {
            while (true)
            {
                SetTreatNewlinesAsTokens(false);

                if (IsNextToken(SyntaxKind.CloseBraceToken, out _))
                    break;

                var armStart = Position;
                var pattern = new PatternSyntaxParser(this).ParsePattern();

                WhenClauseSyntax? whenClause = null;
                if (ConsumeToken(SyntaxKind.WhenKeyword, out var whenKeyword))
                {
                    var condition = new ExpressionSyntaxParser(this).ParseExpression();
                    whenClause = WhenClause(whenKeyword, condition);
                }

                ConsumeTokenOrMissing(SyntaxKind.FatArrowToken, out var arrowToken);

                var previousTreatNewlinesDuringExpression = TreatNewlinesAsTokens;
                SetTreatNewlinesAsTokens(true);

                var expression = new ExpressionSyntaxParser(this).ParseExpression();

                SyntaxToken terminatorToken;
                if (!ConsumeToken(SyntaxKind.CommaToken, out terminatorToken))
                {
                    TryConsumeTerminator(out terminatorToken);
                }

                SetTreatNewlinesAsTokens(previousTreatNewlinesDuringExpression);

                SetTreatNewlinesAsTokens(false);

                if (Position == armStart)
                {
                    var bad = PeekToken();
                    var tokenText = string.IsNullOrEmpty(bad.Text) ? bad.Kind.ToString() : bad.Text;

                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                            GetSpanOfPeekedToken(),
                            tokenText));

                    if (bad.IsKind(SyntaxKind.CloseBraceToken) || bad.IsKind(SyntaxKind.EndOfFileToken))
                        break;

                    ReadToken();
                    continue;
                }

                arms.Add(MatchArm(pattern, whenClause, arrowToken, expression, terminatorToken));
            }
        }
        finally
        {
            ExitParens();
        }

        SetTreatNewlinesAsTokens(previousTreatNewlinesAsTokens);

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        SetTreatNewlinesAsTokens(false);

        return MatchExpression(scrutinee, matchKeyword, openBraceToken, List(arms.ToArray()), closeBraceToken);
    }

    private void SkipMatchArmSeparators()
    {
        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.NewLineToken or SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken)
            {
                ReadToken();
                continue;
            }

            break;
        }
    }

    private IfExpressionSyntax ParseIfExpressionSyntax()
    {
        var ifKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this, stopOnOpenBrace: true).ParseExpression();

        var afterCloseParen = GetEndOfLastToken();

        if (condition.IsMissing)
        {
            AddDiagnostic(
               DiagnosticInfo.Create(
                   CompilerDiagnostics.InvalidExpressionTerm,
                   GetStartOfLastToken(),
                   [')']
               ));
        }

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        if (expression!.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    afterCloseParen
                ));
        }

        ElseClauseSyntax? elseClause = null;

        var elseToken = PeekToken();

        if (elseToken.IsKind(SyntaxKind.ElseKeyword))
        {
            elseClause = ParseElseClauseSyntax();
        }

        return IfExpression(ifKeyword, condition!, expression!, elseClause, Diagnostics);
    }

    private ElseClauseSyntax ParseElseClauseSyntax()
    {
        var elseKeyword = ReadToken();

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        return ElseClause(elseKeyword, expression);
    }

    internal ArrowExpressionClauseSyntax? ParseArrowExpressionClause()
    {
        var arrowToken = ReadToken();

        var previous = TreatNewlinesAsTokens;

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(previous);

        return ArrowExpressionClause(arrowToken, expression);
    }


    private static ExpressionSyntax ParseExpressionFromText(string text)
    {
        var parser = new LanguageParser(null, new Raven.CodeAnalysis.ParseOptions());
        return (ExpressionSyntax)parser.ParseSyntax(typeof(Raven.CodeAnalysis.Syntax.ExpressionSyntax), SourceText.From(text), 0)!;
    }

    private ArgumentListSyntax CreateMissingArgumentList()
    {
        // Used for `TypeName { ... }` where there are no parentheses.
        // We synthesize an empty, missing argument list so the tree remains structurally consistent.
        var openParen = MissingToken(SyntaxKind.OpenParenToken);
        var closeParen = MissingToken(SyntaxKind.CloseParenToken);
        return ArgumentList(openParen, List(Array.Empty<GreenNode>()), closeParen, Diagnostics);
    }

    private WithExpressionSyntax ParseWithExpression(ExpressionSyntax expression)
    {
        // We assume current token is `with`
        var withKeyword = ReadToken();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        var previousTreatNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        EnterParens();
        try
        {
            var assignments = new List<WithAssignmentSyntax>();

            while (true)
            {
                SetTreatNewlinesAsTokens(false);

                if (IsNextToken(SyntaxKind.CloseBraceToken, out _) || PeekToken().IsKind(SyntaxKind.EndOfFileToken))
                    break;

                var entryStart = Position;
                var assignment = ParseWithAssignment();

                if (Position == entryStart)
                {
                    // No progress: consume one token to avoid infinite loop.
                    var bad = PeekToken();
                    AddDiagnostic(DiagnosticInfo.Create(
                        CompilerDiagnostics.InvalidExpressionTerm,
                        GetSpanOfPeekedToken(),
                        bad.Text));
                    ReadToken();
                    continue;
                }

                assignments.Add(assignment);
            }

            SetTreatNewlinesAsTokens(previousTreatNewlinesAsTokens);

            ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

            SetTreatNewlinesAsTokens(false);

            return WithExpression(expression, withKeyword, openBraceToken, List(assignments.ToArray()), closeBraceToken);
        }
        finally
        {
            ExitParens();
            SetTreatNewlinesAsTokens(previousTreatNewlinesAsTokens);
        }
    }

    private WithAssignmentSyntax ParseWithAssignment()
    {
        // Entry kind is decided by lookahead: <identifier> '=' ...
        if (!CanTokenBeIdentifier(PeekToken()))
        {
            ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var missingIdentifier);
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.IdentifierExpected,
                    GetEndOfLastToken()));

            var missingName = IdentifierName(missingIdentifier);

            ConsumeTokenOrMissing(SyntaxKind.EqualsToken, out var missingEquals);

            var missingExpr = new ExpressionSyntax.Missing();

            // Optional terminator
            SyntaxToken terminatorToken2 = Token(SyntaxKind.None);
            if (ConsumeToken(SyntaxKind.CommaToken, out var comma))
                terminatorToken2 = comma;
            else if (!IsNextToken(SyntaxKind.CloseBraceToken, out _))
                TryConsumeTerminator(out terminatorToken2);

            return WithAssignment(missingName, missingEquals, missingExpr, terminatorToken2);
        }

        var nameToken = ReadToken();
        if (nameToken.Kind != SyntaxKind.IdentifierToken)
        {
            nameToken = ToIdentifierToken(nameToken);
            UpdateLastToken(nameToken);
        }

        var name = IdentifierName(nameToken);

        ConsumeTokenOrMissing(SyntaxKind.EqualsToken, out var equalsToken);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(true);

        // Terminator is optional: comma, newline/semicolon, or nothing before `}`
        SyntaxToken terminatorToken = Token(SyntaxKind.None);

        if (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
        {
            terminatorToken = commaToken;
        }
        else if (!IsNextToken(SyntaxKind.CloseBraceToken, out _))
        {
            TryConsumeTerminator(out terminatorToken);
        }

        return WithAssignment(name, equalsToken, expression, terminatorToken);
    }

    private ObjectInitializerExpressionSyntax ParseObjectInitializerExpression()
    {
        // We assume current token is '{'
        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        var previousTreatNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        EnterParens();
        try
        {
            var entries = new List<ObjectInitializerEntrySyntax>();

            while (true)
            {
                SetTreatNewlinesAsTokens(false);

                var token = PeekToken();

                if (IsNextToken(SyntaxKind.CloseBraceToken, out _) || PeekToken().IsKind(SyntaxKind.EndOfFileToken))
                    break;

                var entryStart = Position;
                var entry = ParseObjectInitializerEntry();

                if (Position == entryStart)
                {
                    // No progress: consume one token and continue (or break) to avoid infinite loop.
                    var bad = PeekToken();
                    AddDiagnostic(DiagnosticInfo.Create(
                        CompilerDiagnostics.InvalidExpressionTerm,
                        GetSpanOfPeekedToken(),
                        bad.Text));
                    ReadToken();
                    continue;
                }

                entries.Add(entry);
            }

            SetTreatNewlinesAsTokens(previousTreatNewlinesAsTokens);

            ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

            SetTreatNewlinesAsTokens(false);

            return ObjectInitializerExpression(openBraceToken, List(entries.ToArray()), closeBraceToken);
        }
        finally
        {
            ExitParens();
            SetTreatNewlinesAsTokens(previousTreatNewlinesAsTokens);
        }
    }

    private ObjectInitializerEntrySyntax ParseObjectInitializerEntry()
    {
        // Entry kind is decided by lookahead: <identifier> '=' ...
        if (CanTokenBeIdentifier(PeekToken()) && PeekToken(1).IsKind(SyntaxKind.EqualsToken))
        {
            var nameToken = ReadToken();
            if (nameToken.Kind != SyntaxKind.IdentifierToken)
            {
                nameToken = ToIdentifierToken(nameToken);
                UpdateLastToken(nameToken);
            }

            var name = IdentifierName(nameToken);
            var equalsToken = ReadToken();

            var expression = new ExpressionSyntaxParser(this).ParseExpression();

            SetTreatNewlinesAsTokens(true);

            SyntaxToken terminatorToken;
            if (!ConsumeToken(SyntaxKind.CommaToken, out terminatorToken))
            {
                TryConsumeTerminator(out terminatorToken);
            }

            return ObjectInitializerAssignmentEntry(name, equalsToken, expression, terminatorToken);
        }
        else
        {
            // Child/content entry: any expression, typically `Button { ... }`
            var expression = new ExpressionSyntaxParser(this).ParseExpression();

            SyntaxToken terminatorToken;
            if (!ConsumeToken(SyntaxKind.CommaToken, out terminatorToken))
                TryConsumeTerminator(out terminatorToken);

            return ObjectInitializerExpressionEntry(expression, terminatorToken);
        }
    }

    private bool LooksLikeLambdaAhead(int startOffset)
    {
        // Fast-path to avoid speculative parsing (checkpoint/rewind) when there's clearly no lambda.
        // We only attempt to parse a lambda if we can see a `=>` before a line break or a hard terminator.
        // This is intentionally conservative: false means "don't try lambda parsing".

        const int MaxLookahead = 64; // keep bounded; lambdas should surface quickly

        var depth = 0; // track (), [], {} nesting so we don't stop on commas inside them

        for (int i = startOffset; i < startOffset + MaxLookahead; i++)
        {
            var t = PeekToken(i);

            if (t.IsKind(SyntaxKind.EndOfFileToken))
                return false;

            // Stop at line breaks.
            // When newlines are treated as tokens, we see them directly.
            // When newlines are treated as trivia, the *next* token carries the newline in its leading trivia.
            if (IsNewLineLike(t))
                return false;

            if (i > startOffset && HasLeadingNewLine(t))
                return false;

            // If we're not nested, these tokens end the current expression/statement region.
            // Note: we intentionally do NOT treat `)` as a terminator here, because parenthesized lambdas
            // have the shape `( ... ) => ...` and we still want to see the `=>` after the `)`.
            if (depth == 0)
            {
                if (t.IsKind(SyntaxKind.SemicolonToken)
                    || t.IsKind(SyntaxKind.CommaToken)
                    || t.IsKind(SyntaxKind.CloseBracketToken)
                    || t.IsKind(SyntaxKind.CloseBraceToken))
                {
                    return false;
                }
            }

            if (t.IsKind(SyntaxKind.FatArrowToken))
                return true;

            if (t.IsKind(SyntaxKind.OpenParenToken)
                || t.IsKind(SyntaxKind.OpenBracketToken)
                || t.IsKind(SyntaxKind.OpenBraceToken))
            {
                depth++;
                continue;
            }

            if (t.IsKind(SyntaxKind.CloseParenToken)
                || t.IsKind(SyntaxKind.CloseBracketToken)
                || t.IsKind(SyntaxKind.CloseBraceToken))
            {
                if (depth > 0)
                    depth--;
                continue;
            }
        }

        return false;
    }
}
