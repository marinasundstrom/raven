using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Parser.Internal;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.ConstrainedExecution;

namespace Raven.CodeAnalysis.Parser;

public class SyntaxParser
{
    private Tokenizer _tokenizer;
    private int _currentSpanPosition = 0;

    public DiagnosticBag Diagnostics { get; }

    public SyntaxParser(DiagnosticBag diagnostics)
    {
        Diagnostics = diagnostics;
    }

    public SyntaxTree Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        _tokenizer = new Tokenizer(textReader);

        var compilationUnit = ParseCompilationUnit();
        return SyntaxTree.Create(sourceText, compilationUnit, Diagnostics);
    }

    private CompilationUnitSyntax ParseCompilationUnit()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        SyntaxToken nextToken;

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            ParseNamespaceMemberDeclarations(importDirectives, memberDeclarations, nextToken);
        }

        return CompilationUnit()
            .WithImports(List(importDirectives.ToArray()))
            .WithMembers(List(memberDeclarations.ToArray()))
            .WithEndOfFileToken(nextToken);
    }

    private MemberDeclarationSyntax ParseNamespaceDeclaration()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        var namespaceKeyword = ReadToken();

        var name = ParseName();

        if (ConsumeToken(SyntaxKind.OpenBraceToken, out var openBraceToken))
        {
            while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken) && !nextToken.IsKind(SyntaxKind.CloseBraceToken))
            {
                ParseNamespaceMemberDeclarations(importDirectives, memberDeclarations, nextToken);
            }

            if (!ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken))
            {
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.CharacterExpected,
                        new Location(
                            new TextSpan(_currentSpanPosition, closeBraceToken.FullWidth)),
                        ["}"]
                    ));
            }

            ConsumeTokenOrNull(SyntaxKind.SemicolonToken, out var semicolonToken);

            return NamespaceDeclaration(namespaceKeyword, name, openBraceToken, List(importDirectives), List(memberDeclarations), closeBraceToken, semicolonToken);
        }

        return ParseFileScopedNamespaceDeclarationCore(importDirectives, memberDeclarations, namespaceKeyword, name);
    }

    private MemberDeclarationSyntax ParseFileScopedNamespaceDeclarationCore(List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations, SyntaxToken namespaceKeyword, NameSyntax name)
    {
        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(_currentSpanPosition, semicolonToken.FullWidth))
                ));
        }

        while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken))
        {
            ParseNamespaceMemberDeclarations(importDirectives, memberDeclarations, nextToken);
        }

        return FileScopedNamespaceDeclaration(namespaceKeyword, name, semicolonToken, List(importDirectives), List(memberDeclarations));
    }

    private void ParseNamespaceMemberDeclarations(List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations, SyntaxToken nextToken)
    {
        if (nextToken.IsKind(SyntaxKind.ImportKeyword))
        {
            var importDirective = ParseImportDirective();
            importDirectives.Add(importDirective);
        }
        else if (nextToken.IsKind(SyntaxKind.NamespaceKeyword))
        {
            var namespaceDeclaration = ParseNamespaceDeclaration();
            memberDeclarations.Add(namespaceDeclaration);
        }
        else
        {
            // Should warn

            var statement = ParseStatementSyntax();
            var globalStatement = GlobalStatement(statement);
            memberDeclarations.Add(globalStatement);
        }
    }

    private ImportDirectiveSyntax ParseImportDirective()
    {
        var importKeyword = ReadToken();

        var namespaceName = ParseName();

        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(_currentSpanPosition, semicolonToken.FullWidth))
                ));
        }

        return ImportDirective(importKeyword, namespaceName, semicolonToken);
    }

    private NameSyntax ParseName()
    {
        NameSyntax left = ParseSimpleName();
        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            left = QualifiedName(left, dotToken, ParseSimpleName());
        }
        return left;
    }

    private StatementSyntax? ParseStatementSyntax()
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

            case SyntaxKind.SemicolonToken:
                ReadToken();
                return EmptyStatement(token);
        }

        return ParseDeclarationOrExpressionStatementSyntax();
    }

    private StatementSyntax? ParseReturnStatementSyntax()
    {
        var returnKeyword = ReadToken();

        var expression = ParseExpressionSyntax();

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(_currentSpanPosition, semicolonToken.FullWidth))
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

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        var declaration = ParseVariableDeclarationSyntax();

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(_currentSpanPosition, semicolonToken.FullWidth))
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
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
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

        ConsumeToken(SyntaxKind.OpenParenToken, out var openParenToken);

        var condition = ParseExpressionSyntax();

        ConsumeToken(SyntaxKind.CloseParenToken, out var closeParenToken);

        var statement = ParseStatementSyntax();

        ElseClauseSyntax? elseClause = null;

        var elseToken = PeekToken();

        if (elseToken.IsKind(SyntaxKind.ElseKeyword))
        {
            elseClause = ParseElseClauseSyntax();
        }

        var ifStatement = IfStatement(ifKeyword, openParenToken, condition!, closeParenToken, statement!);

        if (ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
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

    private BlockSyntax? ParseBlockSyntax()
    {
        if (ConsumeToken(SyntaxKind.OpenBraceToken, out var openBraceToken))
        {
            var statements = ParseStatementsList(SyntaxKind.CloseBraceToken, out var closeBraceToken);

            return Block(openBraceToken, statements, closeBraceToken);
        }

        return null;
    }

    private SyntaxList<StatementSyntax> ParseStatementsList(SyntaxKind untilToken, out SyntaxToken token)
    {
        List<StatementSyntax> statements = [];

        while (!ConsumeToken(untilToken, out token))
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

            case SyntaxKind.EqualsToken:
                return SyntaxKind.EqualsExpression;

            case SyntaxKind.NotEqualsToken:
                return SyntaxKind.NotEqualsExpression;

            case SyntaxKind.LessThanEqualsToken:
                return SyntaxKind.LessThanExpression;

            case SyntaxKind.GreaterThanToken:
                return SyntaxKind.GreaterThanExpression;

            case SyntaxKind.LessThanOrEqualExpression:
                return SyntaxKind.LessThanOrEqualExpression;

            case SyntaxKind.GreaterOrEqualsToken:
                return SyntaxKind.GreaterThanOrEqualExpression;

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
        ExpressionSyntax ret = ParseNotExpression();
        SyntaxToken token;
        while (ConsumeToken(SyntaxKind.AndToken, out token))
        {
            ret = BinaryExpression(SyntaxKind.LogicalAndExpression, ret, token, ParseAndExpression());
        }
        return ret;
    }

    private ExpressionSyntax ParseNotExpression()
    {
        if (ConsumeToken(SyntaxKind.NotKeyword, out var token))
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

        if (ConsumeToken(SyntaxKind.CaretToken, out var token))
        {
            ExpressionSyntax right = ParseFactorExpression();
            expr = BinaryExpression(SyntaxKind.PowerExpression, expr, token, right);
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

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

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
        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            expr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, dotToken, ParseSimpleName());
        }

        return expr;
    }

    private ExpressionSyntax ParseParenthesisExpression()
    {
        var openParenToken = ReadToken();

        var expr = ParseExpressionSyntax();

        if (!ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken))
        {
            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    new Location(
                        new TextSpan(_currentSpanPosition, closeParenToken.FullWidth))
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

    private bool IsNextToken(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = PeekToken();
        if (token.Kind == kind)
        {
            return true;
        }
        return false;
    }

    private bool ConsumeToken(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = PeekToken();
        if (token.Kind == kind)
        {
            ReadTokenCore();
            return true;
        }
        return false;
    }

    private bool ConsumeTokenOrMissing(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        var hasConsumedToken = ConsumeToken(kind, out token);
        if (!hasConsumedToken)
        {
            token = MissingToken(kind);
            return false;
        }
        return true;
    }

    private bool ConsumeTokenOrNull(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken? token)
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

    private SyntaxToken PeekToken() => _tokenizer.PeekToken();

    private SyntaxToken ReadToken()
    {
        return ReadTokenCore();
    }

    private SyntaxToken ReadTokenCore()
    {
        var token = _tokenizer.ReadToken();
        _currentSpanPosition += token.FullWidth;
        return token;
    }

    public SyntaxNode? ParseSyntax(Type requestedSyntaxType, SourceText sourceText, int position)
    {
        using var textReader = sourceText.GetTextReader(position);

        _tokenizer = new Tokenizer(textReader);

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
        _currentSpanPosition = position;
    }
}
