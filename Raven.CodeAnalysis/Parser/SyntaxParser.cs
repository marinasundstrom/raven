using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Parser.Internal;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;
using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Parser;

public class SyntaxParser
{
    private Tokenizer tokenizer;
    private int currentSpanPosition = 0;
    private readonly DiagnosticBag _diagnosticBag;

    public DiagnosticBag DiagnosticBag => _diagnosticBag;

    public SyntaxParser(DiagnosticBag diagnosticBag)
    {
        _diagnosticBag = diagnosticBag;
    }

    public SyntaxTree Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        tokenizer = new Tokenizer(new Lexer(textReader));

        var compilationUnit = ParseCompilationUnit();
        return SyntaxTree.Create(sourceText, compilationUnit, DiagnosticBag);
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
        var token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.OpenBraceToken:
                return ParseBlockSyntax();

            case SyntaxKind.IfKeyword:
                return ParseIfStatementSyntax();

            case SyntaxKind.ReturnKeyword:
                return ParseReturnStatementSyntax();
        }

        return ParseDeclarationOrExpressionStatementSyntax();
    }

    private StatementSyntax? ParseReturnStatementSyntax()
    {
        var returnKeyword = ReadToken();

        var expression = ParseExpressionSyntax();

        if (!Consume(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            DiagnosticBag.Add(
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
        var token = PeekToken();

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

            DiagnosticBag.Add(
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

        EqualsValueClauseSyntax? initializer = null;

        var typeAnnotation = ParseTypeAnnotationSyntax();

        if (PeekToken().IsKind(SyntaxKind.EqualsToken))
        {
            initializer = ParseEqualsValueSyntax();
        }

        var declarators = SeparatedList<VariableDeclaratorSyntax>(
            VariableDeclarator(name, typeAnnotation, initializer));

        return VariableDeclaration(letKeyword, declarators);
    }

    private TypeAnnotationSyntax? ParseTypeAnnotationSyntax()
    {
        if (Consume(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = ParseSimpleName();

            return TypeAnnotation(colonToken, type);
        }

        return null;
    }

    private EqualsValueClauseSyntax? ParseEqualsValueSyntax()
    {
        var equalsToken = ReadToken();

        var expr = ParseExpressionSyntax();

        if (expr is null)
        {
            /*
            DiagnosticBag.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.ExpressionExpected,
                    new Location(
                        new TextSpan(currentSpanPosition, 1))
                ));
            */
        }

        return new EqualsValueClauseSyntax(equalsToken, expr);
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

        var elseToken = PeekToken();

        if (elseToken.IsKind(SyntaxKind.ElseKeyword))
        {
            elseClause = ParseElseClauseSyntax();
        }

        var ifStatement = IfStatement(ifKeyword, openParenToken, condition!, closeParenToken, statement!);

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
        return ParseOrExpression();
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

    private ExpressionSyntax ParseOrExpression()
    {
        ExpressionSyntax ret = ParseAndExpression();
        SyntaxToken token;
        while (Consume(SyntaxKind.OrToken, out token))
        {
            ret = BinaryExpression(ret, token, ParseAndExpression());
        }
        return ret;
    }

    private ExpressionSyntax ParseAndExpression()
    {
        ExpressionSyntax ret = ParseNotExpression();
        SyntaxToken token;
        while (Consume(SyntaxKind.AndToken, out token))
        {
            ret = BinaryExpression(ret, token, ParseAndExpression());
        }
        return ret;
    }

    private ExpressionSyntax ParseNotExpression()
    {
        if (Consume(SyntaxKind.NotKeyword, out var token))
        {
            ExpressionSyntax ret = new UnaryExpressionSyntax(token, ParseNotExpression());
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
    internal ExpressionSyntax ParseComparisonExpression()
    {
        ExpressionSyntax expr = ParseExpressionCore(0);
        while (true)
        {
            var token = PeekToken();

            switch (token.Kind)
            {
                case SyntaxKind.GreaterThanToken:
                case SyntaxKind.LessThanToken:
                case SyntaxKind.GreaterOrEqualsToken:
                case SyntaxKind.LessThanEqualsToken:
                case SyntaxKind.EqualsToken:
                case SyntaxKind.NotEqualsToken:
                    ReadToken();
                    break;
                default:
                    return expr;
            }
            ExpressionSyntax rhs = ParseComparisonExpression();
            expr = BinaryExpression(expr, token, rhs);
        }
    }

    /// <summary>
    /// Parse an ExpressionSyntax (Internal)
    /// </summary>
    /// <returns>An expression.</returns>
    /// <param name="precedence">The current level of precedence.</param>
    private ExpressionSyntax ParseExpressionCore(int precedence)
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
                expr = BinaryExpression(expr, operatorCandidate, right);
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
        switch (candidateToken.Kind)
        {
            case SyntaxKind.PercentToken:
            case SyntaxKind.StarToken:
            case SyntaxKind.SlashToken:
                precedence = 5;
                break;

            case SyntaxKind.PlusToken:
            case SyntaxKind.MinusToken:
                //case SyntaxKind.DashToken:
                precedence = 4;
                break;

            /*
            case SyntaxKind.EqualsEqualsToken:
            case SyntaxKind.BangEqualsToken: */
            case SyntaxKind.LessThanToken:
            case SyntaxKind.LessThanEqualsToken:
            case SyntaxKind.GreaterThanToken:
            case SyntaxKind.GreaterOrEqualsToken:
                precedence = 3;
                break;

            /*
            case SyntaxKind.AmpersandToken:
            case SyntaxKind.AmpersandAmpersandToken:
                return 2;

            case SyntaxKind.PipeToken:
            case SyntaxKind.PipePipeToken:
            case SyntaxKind.HatToken:
                return 1;
                */

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
    private ExpressionSyntax ParseFactorExpression()
    {
        ExpressionSyntax expr;

        SyntaxToken token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.PlusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(token, expr);
                break;

            case SyntaxKind.MinusToken:
                ReadToken();
                expr = ParseFactorExpression();
                expr = UnaryExpression(token, expr);
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
    private ExpressionSyntax ParsePowerExpression()
    {
        ExpressionSyntax expr = ParsePrimaryExpression();

        expr = AddTrailers(expr);

        if (Consume(SyntaxKind.CaretToken, out var token))
        {
            ExpressionSyntax right = ParseFactorExpression();
            expr = BinaryExpression(expr, token, right);
        }

        return expr;
    }

    private ExpressionSyntax AddTrailers(ExpressionSyntax expr)
    {
        while (true) // Loop to handle consecutive member access and invocations
        {
            var token = PeekToken();

            if (token.Kind == SyntaxKind.OpenParenToken) // Invocation
            {
                var argumentList = ParseArgumentListSyntax();
                expr = InvocationExpression(expr, argumentList);
            }
            else if (token.Kind == SyntaxKind.DotToken) // Member Access
            {
                var dotToken = ReadToken();
                var memberName = ParseSimpleName();
                expr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, dotToken, memberName);
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

        List<SyntaxNodeOrToken> argumentList = new List<SyntaxNodeOrToken>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            var expression = ParseExpressionSyntax();
            if (expression is null)
                break;

            argumentList.Add(Argument(expression));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                argumentList.Add(commaToken);
            }
        }

        ConsumeOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ArgumentList(openParenToken, SeparatedList<ArgumentSyntax>(argumentList.ToArray()), closeParenToken);
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
            case SyntaxKind.IdentifierToken:
                return ParseIdentifierNameSyntax();

            //return ParseIdentifierNameSyntax();
            /*
                expr = ParserNameOrMemberAccess(
                    ParseIdentifierNameSyntax()
                );

                if (PeekToken().Kind == SyntaxKind.OpenParenToken)
                {
                    expr = InvocationExpression(expr, ParseArgumentListSyntax());
                }
            break;  */

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

            case SyntaxKind.OpenParenToken:
                expr = ParseParenthesisExpression();
                break;
        }

        return expr;
    }

    private ExpressionSyntax ParserNameOrMemberAccess(ExpressionSyntax? expr = null)
    {
        expr ??= ParseIdentifierNameSyntax();
        while (Consume(SyntaxKind.DotToken, out var dotToken))
        {
            expr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, dotToken, ParseSimpleName());
        }

        return expr;
    }

    private ExpressionSyntax ParseParenthesisExpression()
    {
        var openParenToken = ReadToken();

        var expr = ParseExpressionSyntax();

        if (!ConsumeOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken))
        {
            DiagnosticBag.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(currentSpanPosition, closeParenToken.FullWidth))
                )); ;
        }

        return ParenthesizedExpression(openParenToken, expr, closeParenToken);
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

    private ExpressionSyntax ParseIdentifierNameSyntax()
    {
        var token = ReadToken();
        if (token.IsKind(SyntaxKind.IdentifierToken))
        {
            return IdentifierName(token);
        }

        throw new Exception();
    }

    /*

    private ExpressionSyntax ParseNumberExpression()
    {
        SyntaxToken token, token2, token3;
        ExpressionSyntax expr = null;

        token = ReadToken();
        if (Consume(SyntaxKind.Period, out token2))
        {
            if (Consume(SyntaxKind.Number, out token3))
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
        token = PeekToken();
        if (token.Kind == kind)
        {
            ReadTokenCore();
            return true;
        }
        return false;
    }

    private bool ConsumeOrMissing(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = PeekToken();
        if (token.Kind == kind)
        {
            ReadTokenCore();
            return true;
        }
        token = MissingToken(kind);
        return false;
    }

    private SyntaxToken PeekToken() => tokenizer.PeekToken();

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
        else if (requestedSyntaxType == typeof(IdentifierNameSyntax))
        {
            return ParseReturnStatementSyntax();
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
