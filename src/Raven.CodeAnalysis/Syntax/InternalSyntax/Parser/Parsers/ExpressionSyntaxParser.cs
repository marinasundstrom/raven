namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class ExpressionSyntaxParser : SyntaxParser
{
    private readonly bool _allowMatchExpressionSuffixes;

    public ExpressionSyntaxParser(ParseContext parent, bool allowMatchExpressionSuffixes = true)
        : base(parent)
    {
        _allowMatchExpressionSuffixes = allowMatchExpressionSuffixes;
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

        if (precedence == 0 && TryParseAssignmentPattern(out var pattern))
        {
            assignmentPattern = pattern;
        }
        else
        {
            expr = ParseFactorExpression();
        }

        if (ConsumeToken(SyntaxKind.EqualsToken, out var assignToken))
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

            return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, leftNode, assignToken, right, Diagnostics);
        }

        while (true)
        {
            var operatorCandidate = PeekToken();

            if (operatorCandidate.IsKind(SyntaxKind.EndOfFileToken))
                return expr;

            int prec;
            if (!TryResolveOperatorPrecedence(operatorCandidate, out prec))
                return expr;

            if (prec >= precedence)
            {
                ReadToken();
                var right = ParseExpressionCore(prec + 1);
                expr = BinaryExpression(GetBinaryExpressionKind(operatorCandidate), expr!, operatorCandidate, right);
            }
            else
            {
                return expr!;
            }
        }
    }

    private bool TryParseAssignmentPattern(out PatternSyntax pattern)
    {
        pattern = null!;

        if (!IsPossibleAssignmentPatternStart(PeekToken()))
            return false;

        var checkpoint = CreateCheckpoint("assignment-pattern");

        var parsedPattern = new PatternSyntaxParser(this).ParsePattern();

        if (!PeekToken().IsKind(SyntaxKind.EqualsToken))
        {
            checkpoint.Dispose();
            return false;
        }

        pattern = parsedPattern;
        return true;
    }

    private static bool IsPossibleAssignmentPatternStart(SyntaxToken token)
    {
        return token.Kind switch
        {
            SyntaxKind.OpenParenToken => true,
            SyntaxKind.LetKeyword => true,
            SyntaxKind.VarKeyword => true,
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

            case SyntaxKind.TryKeyword:
                expr = ParseTryExpression();
                break;

            case SyntaxKind.OpenBraceToken:
                expr = ParseBlockSyntax();
                break;

            case SyntaxKind.FuncKeyword:
                expr = ParseLambdaExpression();
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
        var expression = new ExpressionSyntaxParser(this, allowMatchExpressionSuffixes: false).ParseExpression();
        return TryExpression(tryKeyword, expression);
    }

    private LambdaExpressionSyntax ParseLambdaExpression()
    {
        var funcKeyword = ReadToken();

        var parameterList = new StatementSyntaxParser(this).ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation()
            ?? ArrowTypeClause(
                SyntaxList.Empty,
                MissingToken(SyntaxKind.ArrowToken),
                IdentifierName(MissingToken(SyntaxKind.IdentifierToken)));

        ConsumeTokenOrMissing(SyntaxKind.FatArrowToken, out var fatArrowToken);

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
                var argumentList = ParseBracketedArgumentListSyntax();

                expr = ElementAccessExpression(expr, argumentList, Diagnostics);
            }
            else if (token.IsKind(SyntaxKind.QuestionToken)) // Conditional access
            {
                var operatorToken = ReadToken();
                ExpressionSyntax whenNotNull;
                var next = PeekToken();
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

                    whenNotNull = MemberBindingExpression(dotToken, memberName);
                    whenNotNull = AddTrailers(start, whenNotNull);
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
            else
            {
                // No more trailers, break out of the loop
                break;
            }
        }

        return expr;
    }

    internal ArgumentListSyntax ParseArgumentListSyntax()
    {
        var openParenToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();
        var seenNames = new HashSet<string>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            if (t.IsKind(SyntaxKind.EndOfFileToken))
                break;

            SetTreatNewlinesAsTokens(false);

            var arg = new ExpressionSyntaxParser(this).ParseArgument();
            if (arg is null)
                break;

            if (arg.NameColon is { } nameColon)
            {
                var name = nameColon.Name.Identifier.GetValueText();
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
            && CanTokenBeIdentifier(PeekToken()))
        {
            var name = ReadToken(); // identifier or keyword
            if (name.Kind != SyntaxKind.IdentifierToken)
            {
                name = ToIdentifierToken(name);
                UpdateLastToken(name);
            }
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

            if (t.IsKind(SyntaxKind.EndOfFileToken))
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

            case SyntaxKind.TypeOfKeyword:
                expr = ParseTypeOfExpression();
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
        checkpoint.Dispose();

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

            CollectionElementSyntax element;

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

    private ExpressionSyntax ParseTypeOfExpression()
    {
        var typeOfKeyword = ReadToken();

        ConsumeTokenOrMissing(SyntaxKind.OpenParenToken, out var openParenToken);

        var type = new NameSyntaxParser(this).ParseTypeName();

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return TypeOfExpression(typeOfKeyword, openParenToken, type, closeParenToken);
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
            expression = ParseMatchExpressionSuffix(expression);
        }

        return expression;
    }

    private MatchExpressionSyntax ParseMatchExpressionSuffix(ExpressionSyntax scrutinee)
    {
        var matchKeyword = ReadToken();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        var arms = new List<MatchArmSyntax>();

        var previousTreatNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        EnterParens();
        try
        {
            while (true)
            {
                SetTreatNewlinesAsTokens(true);

                SkipMatchArmSeparators();

                if (IsNextToken(SyntaxKind.CloseBraceToken, out _))
                    break;

                var pattern = new PatternSyntaxParser(this).ParsePattern();

                WhenClauseSyntax? whenClause = null;
                if (ConsumeToken(SyntaxKind.WhenKeyword, out var whenKeyword))
                {
                    var condition = new ExpressionSyntaxParser(this).ParseExpression();
                    whenClause = WhenClause(whenKeyword, condition);
                }

                ConsumeTokenOrMissing(SyntaxKind.FatArrowToken, out var arrowToken);

                var expression = new ExpressionSyntaxParser(this).ParseExpression();

                TryConsumeTerminator(out var terminatorToken);

                SetTreatNewlinesAsTokens(false);

                arms.Add(MatchArm(pattern, whenClause, arrowToken, expression, terminatorToken));
            }
        }
        finally
        {
            ExitParens();
            SetTreatNewlinesAsTokens(previousTreatNewlinesAsTokens);
        }

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

    internal ArrowExpressionClauseSyntax? ParseArrowExpressionClause()
    {
        var arrowToken = ReadToken();

        var previous = TreatNewlinesAsTokens;

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(previous);

        return ArrowExpressionClause(arrowToken, expression);
    }

    private InterpolatedStringExpressionSyntax ParseInterpolatedStringExpression(SyntaxToken token)
    {
        var contents = new List<InterpolatedStringContentSyntax>();
        var raw = token.Text;
        var inner = raw.Length >= 2 ? raw.Substring(1, raw.Length - 2) : string.Empty;

        int segmentStart = 0;
        for (int i = 0; i < inner.Length;)
        {
            if (IsInterpolationStart(inner, i))
            {
                if (i > segmentStart)
                {
                    var segmentRaw = inner.Substring(segmentStart, i - segmentStart);
                    AddTextSegment(segmentRaw);
                }

                var dollarToken = new SyntaxToken(SyntaxKind.DollarToken, "$");
                var openBraceToken = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");

                i += 2; // skip ${
                int depth = 1;
                int start = i;
                while (i < inner.Length && depth > 0)
                {
                    if (inner[i] == '{')
                        depth++;
                    else if (inner[i] == '}')
                        depth--;
                    i++;
                }

                int end = i - 1;
                var exprText = end >= start ? inner.Substring(start, end - start) : string.Empty;
                var exprSyntax = ParseExpressionFromText(exprText);
                var closeBraceToken = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");
                contents.Add(Interpolation(dollarToken, openBraceToken, exprSyntax, closeBraceToken));

                segmentStart = i;
            }
            else
            {
                i++;
            }
        }

        if (segmentStart < inner.Length)
        {
            var segmentRaw = inner.Substring(segmentStart);
            AddTextSegment(segmentRaw);
        }

        var startToken = new SyntaxToken(
            SyntaxKind.StringStartToken,
            "\"",
            token.LeadingTrivia,
            SyntaxTriviaList.Empty);

        var endToken = new SyntaxToken(
            SyntaxKind.StringEndToken,
            "\"",
            SyntaxTriviaList.Empty,
            token.TrailingTrivia);

        return InterpolatedStringExpression(startToken, List(contents), endToken);

        void AddTextSegment(string segmentRaw)
        {
            if (segmentRaw.Length == 0)
            {
                return;
            }

            var decoded = SyntaxFacts.DecodeStringLiteralContent(segmentRaw.AsSpan(), out _);
            var interned = string.Intern(decoded);
            contents.Add(InterpolatedStringText(new SyntaxToken(
                SyntaxKind.StringLiteralToken,
                interned,
                interned,
                segmentRaw.Length)));
        }
    }

    private static bool ContainsInterpolation(string text)
    {
        for (int i = 0; i < text.Length - 1; i++)
        {
            if (text[i] == '$' && text[i + 1] == '{' && !IsEscaped(text, i))
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsInterpolationStart(string text, int index)
    {
        return index + 1 < text.Length && text[index] == '$' && text[index + 1] == '{' && !IsEscaped(text, index);
    }

    private static bool IsEscaped(string text, int index)
    {
        int backslashCount = 0;
        for (int i = index - 1; i >= 0 && text[i] == '\\'; i--)
        {
            backslashCount++;
        }

        return (backslashCount & 1) == 1;
    }

    private static ExpressionSyntax ParseExpressionFromText(string text)
    {
        var parser = new LanguageParser(null, new Raven.CodeAnalysis.ParseOptions());
        return (ExpressionSyntax)parser.ParseSyntax(typeof(Raven.CodeAnalysis.Syntax.ExpressionSyntax), SourceText.From(text), 0)!;
    }
}
