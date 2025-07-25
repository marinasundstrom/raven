namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class ExpressionSyntaxParser : SyntaxParser
{
    public ExpressionSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    public ExpressionSyntaxParser ParentExpression => (ExpressionSyntaxParser)Parent!;

    public ExpressionSyntax ParseExpression()
    {
        return ParseOrExpression() ?? new ExpressionSyntax.Missing();
    }

    public ExpressionSyntax ParseExpressionOrNull()
    {
        return ParseOrExpression();
    }

    public BlockSyntax ParseBlockSyntax()
    {
        var openBrace = ExpectToken(SyntaxKind.OpenBraceToken);

        EnterParens(); // Treat block as a nesting construct
        var statements = new List<StatementSyntax>();

        while (!IsNextToken(SyntaxKind.CloseBraceToken, out _))
        {
            var stmt = new StatementSyntaxParser(this).ParseStatement();
            if (stmt is not null)
                statements.Add(stmt);

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
            var statement = new StatementSyntaxParser(this).ParseStatement();
            statements.Add(statement);
        }

        return new SyntaxList(statements.ToArray());
    }

    private ExpressionSyntax ParseOrExpression()
    {
        ExpressionSyntax ret = ParseAndExpression();
        SyntaxToken token;
        while (ConsumeToken(SyntaxKind.OrToken, out token))
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

                /*
                case SyntaxKind.LogicalAndToken:
                    return SyntaxKind.LogicalAndExpression;

                case SyntaxKind.LogicalOrToken:
                    return SyntaxKind.LogicalOrExpression;
                */
        }

        throw new ArgumentException("Kind is not valid for this expression.");
    }

    private ExpressionSyntax ParseAndExpression()
    {
        ExpressionSyntax ret = ParseLogicalNotExpression();
        SyntaxToken token;
        while (ConsumeToken(SyntaxKind.AndToken, out token))
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

        var expr = ParseFactorExpression();

        if (ConsumeToken(SyntaxKind.EqualsToken, out var assignToken))
        {
            if (expr is not IdentifierNameSyntax
                and not MemberAccessExpressionSyntax
                and not ElementAccessExpressionSyntax)
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.IdentifierExpected,
                        GetActualTextSpan(start, expr)
                    ));
            }

            var right = ParseExpressionCore(0);

            return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, expr, assignToken, right, Diagnostics);
        }

        while (true)
        {
            var operatorCandidate = PeekToken();

            int prec;
            if (!TryResolveOperatorPrecedence(operatorCandidate, out prec))
                return expr;

            if (prec >= precedence)
            {
                ReadToken();
                var right = ParseExpressionCore(prec + 1);
                expr = BinaryExpression(GetBinaryExpressionKind(operatorCandidate), expr, operatorCandidate, right);
            }
            else
            {
                return expr;
            }
        }
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
        ExpressionSyntax expr;

        SyntaxToken token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.PlusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.UnaryPlusExpression, token, expr);
                break;

            case SyntaxKind.MinusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.UnaryMinusExpression, token, expr);
                break;

            case SyntaxKind.AmpersandToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(SyntaxKind.AddressOfExpression, token, expr);
                break;

            case SyntaxKind.IfKeyword:
                expr = ParseIfExpressionSyntax();
                break;

            case SyntaxKind.WhileKeyword:
                expr = ParseWhileExpressionSyntax();
                break;

            case SyntaxKind.OpenBraceToken:
                expr = ParseBlockSyntax();
                break;

            case SyntaxKind.FuncKeyword:
                expr = ParseLambdaExpression();
                break;

            default:
                return ParsePowerExpression();
        }

        return expr;
    }

    private LambdaExpressionSyntax ParseLambdaExpression()
    {
        var funcKeyword = ReadToken();

        var parameterList = new StatementSyntaxParser(this).ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        if (!ConsumeToken(SyntaxKind.ArrowToken, out var fatArrowToken))
        {

        }

        var body = new ExpressionSyntaxParser(this).ParseExpression();

        return ParenthesizedLambdaExpression(
            funcKeyword,
            parameterList,
            returnParameterAnnotation,
            fatArrowToken,
            body
        );
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

            if (token.IsKind(SyntaxKind.OpenParenToken)) // Invocation
            {
                var argumentList = ParseArgumentListSyntax();
                expr = InvocationExpression(expr, argumentList);
            }
            else if (token.IsKind(SyntaxKind.DotToken)) // Member Access
            {
                var dotToken = ReadToken();
                var memberName = new NameSyntaxParser(this).ParseSimpleName();
                expr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, dotToken, memberName);
            }
            else if (token.IsKind(SyntaxKind.OpenBracketToken)) // Element access
            {
                var argumentList = ParseBracketedArgumentListSyntax();

                /*
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CannotApplyIndexingWithToAnExpressionOfType,
                        GetActualTextSpan(start, expr),
                        [expr.Kind]
                    ));
                    */

                expr = ElementAccessExpression(expr, argumentList, Diagnostics);
            }
            else
            {
                // No more trailers, break out of the loop
                break;
            }
        }

        return expr;
    }

    private ArgumentListSyntax ParseArgumentListSyntax()
    {
        var openParenToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();
        var seenNames = new HashSet<string>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            SetTreatNewlinesAsTokens(false);

            var arg = new ExpressionSyntaxParser(this).ParseArgument();
            if (arg is null)
                break;

            if (arg.GetSlot(0) is { } nameColon)
            {
                var name = nameColon.GetSlot(0).GetSlot(0).GetValueText();
                if (!seenNames.Add(name))
                {
                    //AddDiagnostic(DiagnosticInfo.Create(
                    //    CompilerDiagnostics.DuplicateNamedArgument,
                    //    nameColon.Name.GetLocation()));
                }
            }

            argumentList.Add(arg);

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                argumentList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ArgumentList(openParenToken, List(argumentList.ToArray()), closeParenToken);
    }

    public ArgumentSyntax ParseArgument()
    {
        NameColonSyntax? nameColon = null;

        // Try to parse optional name:
        if (PeekToken(1).IsKind(SyntaxKind.ColonToken)
            && PeekToken().IsKind(SyntaxKind.IdentifierToken))
        {
            var name = ReadToken(); // identifier
            var colon = ReadToken(); // colon
            nameColon = NameColon(IdentifierName(name), colon);
        }

        var expr = ParseExpression();
        return Argument(nameColon, expr);
    }

    private BracketedArgumentListSyntax ParseBracketedArgumentListSyntax()
    {
        var openBracketToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBracketToken))
                break;

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
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);

        return BracketedArgumentList(openBracketToken, List(argumentList.ToArray()), closeBracketToken);
    }

    private TupleExpressionSyntax ParseTupleExpressionSyntax()
    {
        var openParenToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

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

        switch (token.Kind)
        {
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.IntKeyword:
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
                expr = LiteralExpression(SyntaxKind.StringLiteralExpression, token);
                break;

            case SyntaxKind.CharacterLiteralToken:
                ReadToken();
                expr = LiteralExpression(SyntaxKind.CharacterLiteralExpression, token);
                break;

            case SyntaxKind.NullKeyword:
                ReadToken();
                expr = LiteralExpression(SyntaxKind.NullLiteralExpression, token);
                break;

            case SyntaxKind.OpenParenToken:
                expr = ParseParenthesisOrTupleExpression();
                break;

            case SyntaxKind.NewKeyword:
                expr = ParseNewExpression();
                break;

            case SyntaxKind.OpenBracketToken:
                expr = ParseCollectionExpression();
                break;
        }

        return expr;
    }

    private ExpressionSyntax ParseParenthesisOrTupleExpression()
    {
        var openParenToken = ReadToken(); // Consumes '('

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

            if (t.IsKind(SyntaxKind.CloseBracketToken))
                break;

            var expression = new ExpressionSyntaxParser(this).ParseExpression();
            if (expression is null)
                break;

            elementList.Add(CollectionElement(expression));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                elementList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);

        return CollectionExpression(openBracketToken, List(elementList), closeBracketToken);
    }

    private ExpressionSyntax ParseNewExpression()
    {
        var newKeyword = ReadToken();

        var typeName = new NameSyntaxParser(this).ParseTypeName();

        return ObjectCreationExpression(newKeyword, typeName, ParseArgumentListSyntax());
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

    private IfExpressionSyntax ParseIfExpressionSyntax()
    {
        var ifKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this).ParseExpression();

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

    private WhileExpressionSyntax ParseWhileExpressionSyntax()
    {
        var whileKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this).ParseExpression();

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

        var statement = new StatementSyntaxParser(this).ParseStatement();

        if (statement!.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    afterCloseParen
                ));
        }

        return WhileExpression(whileKeyword, condition!, statement!, Diagnostics);
    }

    internal ArrowExpressionClauseSyntax? ParseArrowExpressionClause()
    {
        var arrowToken = ReadToken();

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(true);

        return ArrowExpressionClause(arrowToken, expression);
    }
}