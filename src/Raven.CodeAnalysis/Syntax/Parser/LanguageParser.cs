using System.Diagnostics.CodeAnalysis;
using System.Linq.Expressions;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Parser;

internal class LanguageParser
{
    private List<InternalDiagnostic> _diagnostics = new();

    private string _filePath = string.Empty;
    private Tokenizer _tokenizer;
    private int _currentPosition = 0;

    public ParseOptions Options { get; }
    public Encoding Encoding { get; }

    private SyntaxToken CurrentToken { get; set; }
    private CompilationUnitSyntax CompilationUnit { get; set; }
    private StatementSyntax LastStatement { get; set; }

    public LanguageParser(string? filePath, ParseOptions options)
    {
        _filePath = filePath ?? string.Empty;
        Options = options ?? new ParseOptions();
    }

    public SyntaxTree Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        _tokenizer = new Tokenizer(textReader, _diagnostics);

        var compilationUnit = ParseCompilationUnit();

        var sourceTree = new SyntaxTree(sourceText, _filePath, Options);

        compilationUnit = compilationUnit
            .WithSyntaxTree(sourceTree);

        sourceTree.AttachSyntaxRoot(compilationUnit);

        sourceTree.AddDiagnostics(CollectDiagnostics(compilationUnit.SyntaxTree));

        return sourceTree;
    }

    private DiagnosticBag CollectDiagnostics(SyntaxTree sourceTree)
    {
        List<Diagnostic> diagnostics = new();

        foreach (var diagnostic in _diagnostics)
        {
            var location = sourceTree.GetLocation(diagnostic.Span);
            diagnostics.Add(Diagnostic.Create(diagnostic.Descriptor, location, diagnostic.Args));
        }

        return new DiagnosticBag(diagnostics);
    }

    private CompilationUnitSyntax ParseCompilationUnit()
    {
        CompilationUnit = CompilationUnit();

        SyntaxToken nextToken;

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken);
        }

        return CompilationUnit
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
                ParseNamespaceMemberDeclarations(nextToken);
            }

            if (!ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken))
            {
                _diagnostics.Add(
                    InternalDiagnostic.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetEndOfLastToken(),
                        ['}']
                    ));
            }

            ConsumeTokenOrNull(SyntaxKind.SemicolonToken, out var semicolonToken);

            return NamespaceDeclaration(namespaceKeyword, name, openBraceToken, List(importDirectives), List(memberDeclarations), closeBraceToken, semicolonToken);
        }

        return ParseFileScopedNamespaceDeclarationCore(namespaceKeyword, name);
    }

    private MemberDeclarationSyntax ParseFileScopedNamespaceDeclarationCore(SyntaxToken namespaceKeyword, NameSyntax name)
    {
        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ));
        }

        var fileScopedNamespaceDeclaration = FileScopedNamespaceDeclaration(namespaceKeyword, name, semicolonToken);

        while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken);
        }

        return fileScopedNamespaceDeclaration;
    }

    private void ParseNamespaceMemberDeclarations(SyntaxToken nextToken)
    {
        if (nextToken.IsKind(SyntaxKind.ImportKeyword))
        {
            var importDirective = ParseImportDirective();

            CompilationUnit = CompilationUnit.WithImports(
                CompilationUnit.Imports.Add(importDirective));
        }
        else if (nextToken.IsKind(SyntaxKind.NamespaceKeyword))
        {
            var namespaceDeclaration = ParseNamespaceDeclaration();

            CompilationUnit = CompilationUnit.WithMembers(
                 CompilationUnit.Members.Add(namespaceDeclaration));
        }
        else
        {
            // Should warn (?)

            var statement = ParseStatementSyntax();

            if (statement is null)
                return;

            LastStatement = statement;
            var globalStatement = GlobalStatement(statement);

            CompilationUnit = CompilationUnit.WithMembers(
                CompilationUnit.Members.Add(globalStatement));
        }
    }

    private ImportDirectiveSyntax ParseImportDirective()
    {
        var importKeyword = ReadToken();

        var namespaceName = ParseName();

        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
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

            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
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

        bool isMissing = false;
        var expression = ParseExpressionSyntax();

        if (expression is null)
        {
            var unexpectedToken = ReadToken();

            var unexpectedTokenNoTrivia = unexpectedToken
                .WithLeadingTrivia()
                .WithTrailingTrivia();

            var span = GetStartOfLastToken();
            var unexpectedTokenLeadingTriviaWidth = unexpectedToken.LeadingTrivia.Width;

            var trailingTrivia = LastStatement?.TrailingTrivia ?? SyntaxTriviaList.Empty;
            IEnumerable<SyntaxTrivia> trivia = [.. trailingTrivia, .. unexpectedToken.LeadingTrivia, Trivia(SkippedTokensTrivia(TokenList(unexpectedTokenNoTrivia))), .. unexpectedToken.TrailingTrivia];

            if (LastStatement is not null)
            {
                CompilationUnit = CompilationUnit.ReplaceNode(
                    LastStatement, LastStatement.WithTrailingTrivia(trivia));
            }

            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.InvalidExpressionTerm,
                    new TextSpan(span.Start + unexpectedTokenLeadingTriviaWidth, span.Length),
                    [unexpectedToken.ValueText]
                ));

            if (LastStatement is null)
            {
                return ExpressionStatement(new ExpressionSyntax.Missing(), MissingToken(SyntaxKind.SemicolonToken).WithTrailingTrivia(trivia));
            }

            return null;
        }

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ));
        }

        return ExpressionStatement(expression, semicolonToken);
    }

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        var declaration = ParseVariableDeclarationSyntax();

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
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

        if (IsNextToken(SyntaxKind.EqualsToken, out var _))
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
            TypeSyntax type = ParseName();

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
                InternalDiagnostic.Create(
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

        var condition = ParseExpressionSyntaxOrMissing();

        if (!ConsumeToken(SyntaxKind.CloseParenToken, out var closeParenToken))
        {

            _diagnostics.Add(
               InternalDiagnostic.Create(
                   CompilerDiagnostics.CharacterExpected,
                   GetEndOfLastToken(),
                   [')']
               ));
        }

        var afterCloseParen = GetEndOfLastToken();

        if (condition.IsMissing)
        {
            _diagnostics.Add(
               InternalDiagnostic.Create(
                   CompilerDiagnostics.InvalidExpressionTerm,
                   GetStartOfLastToken(),
                   [')']
               ));
        }

        var statement = ParseStatementSyntax();

        if (statement!.IsMissing)
        {
            _diagnostics.Add(
                InternalDiagnostic.Create(
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

    private ExpressionSyntax ParseExpressionSyntaxOrMissing()
    {
        return ParseOrExpression() ?? new ExpressionSyntax.Missing();
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

            case SyntaxKind.LessThanToken:
                return SyntaxKind.LessThanExpression;

            case SyntaxKind.GreaterThanToken:
                return SyntaxKind.GreaterThanExpression;

            case SyntaxKind.LessThanEqualsToken:
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
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.IntKeyword:
                return ParsePredefinedTypeSyntax();

            case SyntaxKind.IdentifierToken:
                return ParseIdentifierNameSyntax();

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

    private ExpressionSyntax ParsePredefinedTypeSyntax()
    {
        var token = ReadToken();
        return PredefinedType(token);
    }

    private ExpressionSyntax ParseParenthesisExpression()
    {
        var openParenToken = ReadToken();

        var expr = ParseExpressionSyntax();

        if (!ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken))
        {
            _diagnostics.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
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
        return IdentifierName(token);
    }

    public SyntaxNode? ParseSyntax(Type requestedSyntaxType, SourceText sourceText, int position)
    {
        using var textReader = sourceText.GetTextReader(position);

        _tokenizer = new Tokenizer(textReader, _diagnostics);

        SetCurrentPosition(position);

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
        else if (requestedSyntaxType == typeof(ExpressionSyntax))
        {
            return ParseExpressionSyntax();
        }
        else if (requestedSyntaxType == typeof(BlockSyntax))
        {
            return ParseBlockSyntax();
        }
        else if (requestedSyntaxType == typeof(ReturnStatementSyntax))
        {
            return ParseReturnStatementSyntax();
        }
        else if (requestedSyntaxType == typeof(NameSyntax))
        {
            return ParseName();
        }
        else if (requestedSyntaxType == typeof(IdentifierNameSyntax))
        {
            return ParseIdentifierNameSyntax();
        }

        throw new NotSupportedException("Syntax not supported");
    }

    private bool IsNextToken(SyntaxKind kind, [NotNullWhen(true)] out SyntaxToken token)
    {
        token = PeekToken();
        if (token.Kind == kind)
        {
            return true;
        }
        return false;
    }

    private bool IsNextToken(SyntaxKind kind)
    {
        var token = PeekToken();
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
        CurrentToken = _tokenizer.ReadToken();
        _currentPosition += CurrentToken.FullWidth;
        return CurrentToken;
    }

    private TextSpan GetStartOfLastToken()
    {
        return new TextSpan(_currentPosition - CurrentToken.FullWidth, 0);
    }

    private TextSpan GetEndOfLastToken()
    {
        return new TextSpan(_currentPosition - CurrentToken.TrailingTrivia.Count, 0);
    }

    private void SetCurrentPosition(int position)
    {
        _currentPosition = position;
    }
}
