using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis.Parser.Internal;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Parser;

public class SyntaxParser
{
    private Tokenizer tokenizer;
    private int currentSpanPosition = 0;
    private readonly DiagnosticBag _diagnosticBag;

    public SyntaxParser(DiagnosticBag diagnosticBag)
    {
        _diagnosticBag = diagnosticBag;
    }

    public SyntaxTree Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        tokenizer = new Tokenizer(new Lexer(textReader));

        var compilationUnit = ParseCompilationUnit();
        return SyntaxTree.Create(sourceText, compilationUnit, _diagnosticBag);
    }

    private CompilationUnitSyntax ParseCompilationUnit()
    {
        List<MemberDeclarationSyntax> memberDeclarations = [];

        while (!Consume(SyntaxKind.EndOfFileToken, out var endOfFileToken))
        {
            var statement = ParseStatementSyntax();
            var globalStatement = GlobalStatement(statement);

            memberDeclarations.Add(globalStatement);
        }

        return CompilationUnit()
            .WithMembers(List(memberDeclarations.ToArray()))
            .WithEndOfFileToken(EndOfFile);
    }

    public StatementSyntax? ParseStatementSyntax()
    {
        var token = tokenizer.PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.OpenBraceToken:
                return ParseBlockSyntax();

            case SyntaxKind.IfKeyword:
                return ParseIfStatementSyntax();

            case SyntaxKind.ReturnKeyword:
                return ParseReturnStatementSyntaxCore();
        }

        return ParseDeclarationOrExpressionStatementSyntax();
    }

    private StatementSyntax? ParseReturnStatementSyntaxCore()
    {
        var returnKeyword = ReadToken();

        var expression = ParseExpressionSyntax();

        if (!Consume(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            _diagnosticBag.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(currentSpanPosition, semicolonToken.FullWidth))
                ));
        }

        return ReturnStatement(returnKeyword, expression, semicolonToken);
    }

    private StatementSyntax? ParseDeclarationOrExpressionStatementSyntax()
    {
        var token = tokenizer.PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.LetKeyword:
                return ParseLocalDeclarationStatementSyntax();
        }

        var expression = ParseExpressionSyntax();
        return ExpressionStatement(expression);
    }

    public LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        var declaration = ParseVariableDeclarationSyntax();

        if (!Consume(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            _diagnosticBag.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(currentSpanPosition, semicolonToken.FullWidth))
                ));
        }

        return LocalDeclarationStatement(declaration, semicolonToken);
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var letKeyword = ReadToken();

        var name = ParseSimpleName();

        var declarators = SeparatedList<VariableDeclaratorSyntax>(
            VariableDeclarator(name));

        return VariableDeclaration(letKeyword, declarators);
    }

    private IdentifierNameSyntax ParseSimpleName()
    {
        var token = ReadToken();
        return new IdentifierNameSyntax(token);
    }

    private IfStatementSyntax? ParseIfStatementSyntax()
    {
        var ifKeyword = ReadToken();

        Consume(SyntaxKind.OpenParenToken, out var openParenToken);

        var condition = ParseExpressionSyntax();

        Consume(SyntaxKind.CloseParenToken, out var closeParenToken);

        var statement = ParseStatementSyntax();

        ElseClauseSyntax? elseClause = null;

        var elseToken = tokenizer.PeekToken();

        if (elseToken.Kind == SyntaxKind.ElseKeyword)
        {
            elseClause = ParseElseClauseSyntax();
        }

        var ifStatement = IfStatement(ifKeyword, openParenToken, condition!, closeParenToken, statement!, null);

        if (Consume(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            ifStatement = ifStatement.WithSemicolonToken(semicolonToken);
        }

        if (elseClause is not null)
        {
            return ifStatement.WithElseClause(elseClause);
        }

        return ifStatement;
    }

    private ElseClauseSyntax? ParseElseClauseSyntax()
    {
        var elseKeyword = ReadToken();

        return new ElseClauseSyntax(elseKeyword, ParseStatementSyntax());
    }

    private ExpressionSyntax? ParseExpressionSyntax()
    {
        var token = ReadToken();
        if (token.IsKind(SyntaxKind.NumericLiteralToken))
        {
            return new NumericLiteralExpressionSyntax(token);
        }
        return new IdentifierNameSyntax(token);
    }

    public BlockSyntax? ParseBlockSyntax()
    {
        if (Consume(SyntaxKind.OpenBraceToken, out var openBraceToken))
        {
            var statements = ParseStatementsList(SyntaxKind.CloseBraceToken, out var closeBraceToken);

            return Block(openBraceToken, statements, closeBraceToken);
        }

        return null;
    }

    private SyntaxList<StatementSyntax> ParseStatementsList(SyntaxKind untilToken, out SyntaxToken token)
    {
        List<StatementSyntax> statements = [];

        while (!Consume(untilToken, out token))
        {
            var statement = ParseStatementSyntax();
            statements.Add(statement);
        }
        return List(statements.ToArray());
    }

    /*

    public Expression ParseExpression()
    {
        return ParseOrExpression();
    }

    private Expression ParseOrExpression()
    {
        Expression ret = ParseAndExpression();
        TokenInfo token;
        while (MaybeEat(TokenKind.Or, out token))
        {
            ret = new BinaryExpression(token, ret, ParseAndExpression());
        }
        return ret;
    }

    private Expression ParseAndExpression()
    {
        Expression ret = ParseNotExpression();
        TokenInfo token;
        while (MaybeEat(TokenKind.And, out token))
        {
            ret = new BinaryExpression(token, ret, ParseAndExpression());
        }
        return ret;
    }

    private Expression ParseNotExpression()
    {
        var token = TokenInfo.Empty;
        if (MaybeEat(TokenKind.NotKeyword, out token))
        {
            Expression ret = new UnaryExpression(token, ParseNotExpression());
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
    internal Expression ParseComparisonExpression()
    {
        Expression expr = ParseExpressionCore(0);
        while (true)
        {
            var token = PeekToken();

            switch (token.Kind)
            {
                case TokenKind.CloseAngleBracket:
                case TokenKind.GreaterOrEqual:
                case TokenKind.Less:
                case TokenKind.OpenAngleBracket:
                case TokenKind.Equal:
                case TokenKind.NotEquals:
                    ReadToken();
                    break;
                default:
                    return expr;
            }
            Expression rhs = ParseComparisonExpression();
            expr = new BinaryExpression(token, expr, rhs);
        }
    }

    /// <summary>
    /// Parse an expression (Internal)
    /// </summary>
    /// <returns>An expression.</returns>
    /// <param name="precedence">The current level of precedence.</param>
    private Expression ParseExpressionCore(int precedence)
    {
        var expr = ParseFactorExpression();

        while (true)
        {
            var operatorCandidate = PeekToken();

            int prec;
            if (!TryResolveOperatorPrecedence(operatorCandidate, out prec))
                return expr;

            ReadToken();

            if (prec >= precedence)
            {
                var right = ParseExpressionCore(prec + 1);
                expr = new BinaryExpression(operatorCandidate, expr, right);
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
    private bool TryResolveOperatorPrecedence(TokenInfo candidateToken, out int precedence)
    {
        switch (candidateToken.Kind)
        {
            case TokenKind.Percent:
            case TokenKind.Slash:
            case TokenKind.Star:
                precedence = 2;
                break;

            case TokenKind.Minus:
            case TokenKind.Plus:
                precedence = 1;
                break;

            default:
                precedence = -1;
                return false;
        }

        return true;
    }

    /// <summary>
    /// Parse a factor expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private Expression ParseFactorExpression()
    {
        Expression expr;

        TokenInfo token = PeekToken();

        switch (token.Kind)
        {
            case TokenKind.Plus:
                ReadToken();
                expr = ParseFactorExpression();
                expr = new UnaryExpression(token, expr);
                break;

            case TokenKind.Minus:
                ReadToken();
                expr = ParseFactorExpression();
                expr = new UnaryExpression(token, expr);
                break;

            default:
                return ParsePowerExpression();
        }

        return expr;
    }

    /// <summary>
    /// Parse a power expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private Expression ParsePowerExpression()
    {
        Expression expr = ParsePrimaryExpression();

        if (MaybeEat(TokenKind.Caret, out var token))
        {
            Expression right = ParseFactorExpression();
            expr = new BinaryExpression(token, expr, right);
        }

        return expr;
    }

    /// <summary>
    /// Parse a primary expression.
    /// </summary>
    /// <returns>An expression.</returns>
    private Expression ParsePrimaryExpression()
    {
        Expression expr = null;

        TokenInfo token;

        token = PeekToken();

        switch (token.Kind)
        {
            case TokenKind.Identifier:
                ReadToken();
                expr = new IdentifierExpression(token);
                break;

            case TokenKind.TrueKeyword:
                ReadToken();
                expr = new TrueLiteralExpression(token);
                break;

            case TokenKind.FalseKeyword:
                ReadToken();
                expr = new FalseLiteralExpression(token);
                break;

            case TokenKind.IfKeyword:
                expr = ParseIfThenElseExpression();
                break;

            case TokenKind.LetKeyword:
                expr = ParseLetExpression();
                break;

            case TokenKind.Number:
                expr = ParseNumberExpression();
                break;

            case TokenKind.OpenParen:
                expr = ParseParenthesisExpression();
                break;

            case TokenKind.EndOfFile:
                ReadToken();
                Diagnostics.AddError(Strings.Error_UnexpectedEndOfFile, token.GetSpan());
                break;
        }

        return expr;
    }

    private Expression ParseParenthesisExpression()
    {
        TokenInfo token, token2;
        Expression expr = null;

        token = ReadToken();
        token2 = PeekToken();
        if (!MaybeEat(TokenKind.CloseParen, out token2))
        {
            expr = ParseExpression();
        }
        if (expr == null)
        {
            Diagnostics.AddError(string.Format(Strings.Error_InvalidExpressionTerm, token2.Value), token2.GetSpan());
        }
        else
        {
            if (!Eat(TokenKind.CloseParen, out token2))
            {
                Diagnostics.AddError(string.Format(Strings.Error_ExpectedToken, ')'), token2.GetSpan());
            }
        }
        return new ParenthesisExpression(token, expr, token2);
    }

    private Expression ParseNumberExpression()
    {
        TokenInfo token, token2, token3;
        Expression expr = null;

        token = ReadToken();
        if (MaybeEat(TokenKind.Period, out token2))
        {
            if (MaybeEat(TokenKind.Number, out token3))
            {
                expr = new RealNumberExpression(token, token2, token3);
            }
            else
            {
                Diagnostics.AddError(string.Format(Strings.Error_UnexpectedToken, token3.Value), token3.GetSpan());
            }
        }
        else
        {
            expr = new IntegerNumberExpression(token);
        }

        return expr;
    }

    */

    private bool Consume(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = tokenizer.PeekToken();
        if (token.Kind == kind)
        {
            ReadTokenCore();
            return true;
        }
        return false;
    }


    private SyntaxToken ReadToken()
    {
        return ReadTokenCore();
    }

    private SyntaxToken ReadTokenCore()
    {
        var token = tokenizer.ReadToken();
        currentSpanPosition += token.FullWidth;
        return token;
    }

    public SyntaxNode? ParseSyntax(Type requestedSyntaxType, SourceText sourceText, int position)
    {
        using var textReader = sourceText.GetTextReader(position);

        tokenizer = new Tokenizer(new Lexer(textReader));

        SetCurrentSpan(position);

        return ParseRequestedType(requestedSyntaxType);
    }

    private SyntaxNode? ParseRequestedType(Type requestedSyntaxType)
    {
        if (requestedSyntaxType == typeof(StatementSyntax))
        {
            return ParseStatementSyntax();
        }
        else if (requestedSyntaxType == typeof(IfStatementSyntax))
        {
            return ParseIfStatementSyntax();
        }
        else if (requestedSyntaxType == typeof(BlockSyntax))
        {
            return ParseBlockSyntax();
        }
        else if (requestedSyntaxType == typeof(ExpressionSyntax))
        {
            return ParseExpressionSyntax();
        }
        else if (requestedSyntaxType == typeof(IdentifierNameSyntax))
        {
            return ParseSimpleName();
        }

        throw new NotSupportedException("Syntax not supported");
    }

    private void SetCurrentSpan(int position)
    {
        currentSpanPosition = position;
    }
}
