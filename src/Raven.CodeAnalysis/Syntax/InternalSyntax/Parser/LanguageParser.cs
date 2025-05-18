using System.Diagnostics.CodeAnalysis;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class LanguageParser
{
    private readonly string _filePath;
    private Tokenizer _tokenizer;

    private int Position { get; set; }

    public ParseOptions Options { get; }
    public Encoding Encoding { get; }

    private SyntaxToken CurrentToken { get; set; }
    private BlockContext Block { get; set; } = new BlockContext();
    private StatementSyntax? LastStatement => Block.LastStatement;

    public LanguageParser(string? filePath, ParseOptions options)
    {
        _filePath = filePath ?? string.Empty;
        Options = options ?? new ParseOptions();
    }

    public SyntaxNode Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        _tokenizer = new Tokenizer(textReader);

        return ParseCompilationUnit();
    }

    private CompilationUnitSyntax ParseCompilationUnit()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        SyntaxToken nextToken;

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);
        }

        return new CompilationUnitSyntax(List(importDirectives), List(memberDeclarations), nextToken);
    }

    private MemberDeclarationSyntax ParseNamespaceDeclaration()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        List<DiagnosticInfo>? diagnostics = null;

        var namespaceKeyword = ReadToken();

        var name = ParseName();

        if (ConsumeToken(SyntaxKind.OpenBraceToken, out var openBraceToken))
        {
            while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken) && nextToken.Kind != SyntaxKind.CloseBraceToken)
            {
                ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);
            }

            if (!ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken))
            {
                Diagnostics(ref diagnostics).Add(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetEndOfLastToken(),
                        ['}']
                    ));
            }

            ConsumeTokenOrNull(SyntaxKind.SemicolonToken, out var semicolonToken);

            return NamespaceDeclaration(
                namespaceKeyword, name, openBraceToken,
                new SyntaxList(importDirectives.ToArray()), new SyntaxList(memberDeclarations.ToArray()),
                closeBraceToken, semicolonToken, diagnostics);
        }

        return ParseFileScopedNamespaceDeclarationCore(namespaceKeyword, name, importDirectives, memberDeclarations);
    }

    IList<DiagnosticInfo> Diagnostics(ref List<DiagnosticInfo>? diagnostics) => diagnostics ??= new List<DiagnosticInfo>();

    private MemberDeclarationSyntax ParseFileScopedNamespaceDeclarationCore(SyntaxToken namespaceKeyword, NameSyntax name, List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        DiagnosticInfo[]? diagnostics = null;

        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            diagnostics = [
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ) ];
        }

        var fileScopedNamespaceDeclaration = FileScopedNamespaceDeclaration(
            namespaceKeyword, name, semicolonToken,
            SyntaxList.Empty, SyntaxList.Empty, diagnostics);

        while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);
        }

        return fileScopedNamespaceDeclaration;
    }

    private void ParseNamespaceMemberDeclarations(SyntaxToken nextToken, List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        if (nextToken.Kind == SyntaxKind.ImportKeyword)
        {
            var importDirective = ParseImportDirective();

            importDirectives.Add(importDirective);
        }
        else if (nextToken.Kind == SyntaxKind.NamespaceKeyword)
        {
            var namespaceDeclaration = ParseNamespaceDeclaration();

            memberDeclarations.Add(namespaceDeclaration);
        }
        else
        {
            // Should warn (?)

            var statement = ParseStatementSyntax();

            if (statement is null)
                return;

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
            return ImportDirective(importKeyword, namespaceName, semicolonToken,
                [DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                )]);
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

    private TypeSyntax ParseTypeName()
    {
        var peek = PeekToken();
        if (IsPredefinedTypeKeyword(peek))
        {
            ReadToken();

            return PredefinedType(peek);
        }

        NameSyntax left = ParseSimpleName();

        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            left = QualifiedName(left, dotToken, ParseSimpleName());
        }

        return left;
    }

    private bool IsPredefinedTypeKeyword(SyntaxToken token)
    {
        switch (token.Kind)
        {
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.IntKeyword:
                return true;
        }

        return false;
    }

    internal StatementSyntax? ParseStatement(SourceText sourceText, int offset = 0, bool consumeFullText = true)
    {
        using var textReader = sourceText.GetTextReader();

        _tokenizer = new Tokenizer(textReader);

        SetPosition(offset);

        return ParseStatementSyntax();
    }

    private StatementSyntax? ParseStatementSyntax()
    {
        var token = PeekToken();

        StatementSyntax? statement;

        switch (token.Kind)
        {
            case SyntaxKind.ReturnKeyword:
                statement = ParseReturnStatementSyntax();
                break;

            case SyntaxKind.SemicolonToken:
                ReadToken();
                statement = EmptyStatement(token);
                break;

            default:
                statement = ParseDeclarationOrExpressionStatementSyntax();
                break;

        }
        //if (statement is not null)
        //{
        Block.Statements.Add(statement);
        //}
        return statement;
    }

    private StatementSyntax? ParseReturnStatementSyntax()
    {
        var returnKeyword = ReadToken();

        var expression = ParseExpressionSyntax();

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            return ReturnStatement(returnKeyword, expression, semicolonToken,
                [DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                )]);

        }

        return ReturnStatement(returnKeyword, expression, semicolonToken);
    }

    private StatementSyntax? ParseDeclarationOrExpressionStatementSyntax()
    {
        List<DiagnosticInfo>? diagnostics = null;

        var token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.LetKeyword:
            case SyntaxKind.VarKeyword:
                return ParseLocalDeclarationStatementSyntax();

                /*
                            case SyntaxKind.IfKeyword:
                                var ifExpr = ParseIfExpressionSyntax();
                                return new ExpressionStatement1Syntax(ifExpr, diagnostics);

                            case SyntaxKind.WhileKeyword:
                                var whileExpr = ParseWhileExpressionSyntax();
                                return new ExpressionStatement1Syntax(whileExpr, diagnostics);
                                */
        }

        var expression = ParseExpressionSyntax();

        if (expression.IsMissing)
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
                var lastTerminal = LastStatement.GetLastToken();

                var oldLast = LastStatement;
                var lastStatement = (StatementSyntax)LastStatement.ReplaceNode(
                    lastTerminal, lastTerminal.WithTrailingTrivia(trivia));

                Block.ReplaceStatement(oldLast, lastStatement);
            }

            // INFO: Remember
            Diagnostics(ref diagnostics).Add(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.InvalidExpressionTerm,
                    new TextSpan(span.Start + unexpectedTokenLeadingTriviaWidth, span.Length),
                    [unexpectedToken.GetValueText()]
                ));

            if (LastStatement is null)
            {
                return ExpressionStatement(new ExpressionSyntax.Missing(), diagnostics);
            }

            return null;
        }

        if (expression is IfExpressionSyntax or WhileExpressionSyntax or BlockSyntax)
        {
            if (ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken2))
            {
                return ExpressionStatementWithSemicolon(expression, semicolonToken2, diagnostics);
            }
            return ExpressionStatement(expression, diagnostics);
        }

        // INFO: Remember
        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            Diagnostics(ref diagnostics).Add(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ));
        }

        return ExpressionStatementWithSemicolon(expression, semicolonToken, diagnostics);
    }

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        List<DiagnosticInfo>? diagnostics = null;

        var declaration = ParseVariableDeclarationSyntax();

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            Diagnostics(ref diagnostics).Add(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ));
        }

        return LocalDeclarationStatement(declaration, semicolonToken, diagnostics);
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var letOrVarKeyword = ReadToken();

        var name = ParseSimpleName();

        EqualsValueClauseSyntax? initializer = null;

        var typeAnnotation = ParseTypeAnnotationSyntax();

        if (IsNextToken(SyntaxKind.EqualsToken, out var _))
        {
            initializer = ParseEqualsValueSyntax();
        }

        var declarators = new SyntaxList(
            [VariableDeclarator(name, typeAnnotation, initializer)]);

        return new VariableDeclarationSyntax(letOrVarKeyword, declarators);
    }

    private TypeAnnotationSyntax? ParseTypeAnnotationSyntax()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = ParseTypeName();

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

        return EqualsValueClause(equalsToken, expr);
    }

    private IdentifierNameSyntax ParseSimpleName()
    {
        var token = ReadToken();
        return IdentifierName(token);
    }

    private IfExpressionSyntax ParseIfExpressionSyntax()
    {
        List<DiagnosticInfo>? diagnostics = null;

        var ifKeyword = ReadToken();

        var condition = ParseExpressionSyntax();

        var afterCloseParen = GetEndOfLastToken();

        if (condition.IsMissing)
        {
            Diagnostics(ref diagnostics).Add(
               DiagnosticInfo.Create(
                   CompilerDiagnostics.InvalidExpressionTerm,
                   GetStartOfLastToken(),
                   [')']
               ));
        }

        var expression = ParseExpressionSyntax();

        if (expression!.IsMissing)
        {
            Diagnostics(ref diagnostics).Add(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    afterCloseParen
                ));
        }

        ElseClauseSyntax? elseClause = null;

        var elseToken = PeekToken();

        if (elseToken.Kind == SyntaxKind.ElseKeyword)
        {
            elseClause = ParseElseClauseSyntax();
        }

        return IfExpression(ifKeyword, condition!, expression!, elseClause, diagnostics);
    }

    private ElseClauseSyntax ParseElseClauseSyntax()
    {
        var elseKeyword = ReadToken();

        return ElseClause(elseKeyword, ParseExpressionSyntax());
    }

    private WhileExpressionSyntax ParseWhileExpressionSyntax()
    {
        List<DiagnosticInfo>? diagnostics = null;

        var whileKeyword = ReadToken();

        var condition = ParseExpressionSyntax();

        var afterCloseParen = GetEndOfLastToken();

        if (condition.IsMissing)
        {
            Diagnostics(ref diagnostics).Add(
               DiagnosticInfo.Create(
                   CompilerDiagnostics.InvalidExpressionTerm,
                   GetStartOfLastToken(),
                   [')']
               ));
        }

        var statement = ParseStatementSyntax();

        if (statement!.IsMissing)
        {
            Diagnostics(ref diagnostics).Add(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    afterCloseParen
                ));
        }

        return WhileStatement(whileKeyword, condition!, statement!, diagnostics);
    }

    private ExpressionSyntax ParseExpressionOrNull()
    {
        return ParseOrExpression();
    }

    private ExpressionSyntax ParseExpressionSyntax()
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

    private SyntaxList ParseStatementsList(SyntaxKind untilToken, out SyntaxToken token)
    {
        Block = new BlockContext(Block);

        while (!ConsumeToken(untilToken, out token))
        {
            var statement = ParseStatementSyntax();
        }

        var block = Block;
        Block = Block.Parent!;

        return new SyntaxList(block.Statements.ToArray());
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
            ExpressionSyntax ret = UnaryExpression(token, ParseNotExpression());
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
        List<DiagnosticInfo>? diagnostics = null;

        int start = this.Position;

        var expr = ParseFactorExpression();

        if (ConsumeToken(SyntaxKind.EqualsToken, out var assignToken))
        {
            if (expr is not IdentifierNameSyntax
                and not MemberAccessExpressionSyntax
                and not ElementAccessExpressionSyntax)
            {
                Diagnostics(ref diagnostics).Add(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.IdentifierExpected,
                        GetActualTextSpan(start, expr)
                    ));
            }

            var right = ParseExpressionCore(0);

            return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, expr, assignToken, right, diagnostics);
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
    /// Get the actual span of a node.
    /// </summary>
    /// <param name="start">The start fullwidth</param>
    /// <param name="node">The given node</param>
    /// <returns>The actual text span</returns>
    private TextSpan GetActualTextSpan(int start, SyntaxNode node)
    {
        var firstToken = node.GetFirstToken();
        return new TextSpan(start + firstToken.LeadingTrivia.Width, node.Width);
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

            case SyntaxKind.IfKeyword:
                expr = ParseIfExpressionSyntax();
                break;

            case SyntaxKind.WhileKeyword:
                expr = ParseWhileExpressionSyntax();
                break;

            case SyntaxKind.OpenBraceToken:
                expr = ParseBlockSyntax();
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
        List<DiagnosticInfo>? diagnostics = null;

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
            else if (token.Kind == SyntaxKind.OpenBracketToken) // Element access
            {
                var argumentList = ParseBracketedArgumentListSyntax();

                /*
                Diagnostics(ref diagnostics).Add(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CannotApplyIndexingWithToAnExpressionOfType,
                        GetActualTextSpan(start, expr),
                        [expr.Kind]
                    ));
                    */

                expr = ElementAccessExpression(expr, argumentList, diagnostics);
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

        while (true)
        {
            var t = PeekToken();

            if (t.Kind == SyntaxKind.CloseParenToken)
                break;

            var expression = ParseExpressionSyntax();
            if (expression is null)
                break;

            argumentList.Add(Argument(expression));

            var commaToken = PeekToken();
            if (commaToken.Kind == SyntaxKind.CommaToken)
            {
                ReadToken();
                argumentList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ArgumentList(openParenToken, List(argumentList.ToArray()), closeParenToken);
    }

    private BracketedArgumentListSyntax ParseBracketedArgumentListSyntax()
    {
        var openBracketToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.Kind == SyntaxKind.CloseBracketToken)
                break;

            var expression = ParseExpressionSyntax();
            if (expression is null)
                break;

            argumentList.Add(Argument(expression));

            var commaToken = PeekToken();
            if (commaToken.Kind == SyntaxKind.CommaToken)
            {
                ReadToken();
                argumentList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);

        return BracketedArgumentList(openBracketToken, List(argumentList.ToArray()), closeBracketToken);
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

            case SyntaxKind.NewKeyword:
                expr = ParseNewExpression();
                break;

            case SyntaxKind.OpenBracketToken:
                expr = ParseCollectionExpression();
                break;
        }

        return expr;
    }

    private ExpressionSyntax ParseCollectionExpression()
    {
        var openBracketToken = ReadToken();

        List<GreenNode> elementList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.Kind == SyntaxKind.CloseBracketToken)
                break;

            var expression = ParseExpressionSyntax();
            if (expression is null)
                break;

            elementList.Add(CollectionElement(expression));

            var commaToken = PeekToken();
            if (commaToken.Kind == SyntaxKind.CommaToken)
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

        return ObjectCreationExpression(newKeyword, ParseTypeName(), ParseArgumentListSyntax());
    }

    private ExpressionSyntax ParsePredefinedTypeSyntax()
    {
        var token = ReadToken();
        return PredefinedType(token);
    }

    private ExpressionSyntax ParseParenthesisExpression()
    {
        List<DiagnosticInfo>? diagnostics = null;

        var openParenToken = ReadToken();

        var expr = ParseExpressionSyntax();

        if (!ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken))
        {
            Diagnostics(ref diagnostics).Add(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                )); ;
        }

        return ParenthesizedExpression(openParenToken, expr, closeParenToken, diagnostics);
    }

    private ExpressionSyntax ParseNumericLiteralExpressionSyntax()
    {
        var token = ReadToken();
        if (token.Kind == SyntaxKind.NumericLiteralToken)
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

        _tokenizer = new Tokenizer(textReader);

        SetPosition(position);

        return ParseRequestedType(requestedSyntaxType);
    }

    private SyntaxNode? ParseRequestedType(Type requestedSyntaxType)
    {
        if (requestedSyntaxType == typeof(Syntax.StatementSyntax))
        {
            return ParseStatementSyntax();
        }
        else if (requestedSyntaxType == typeof(Syntax.ExpressionSyntax))
        {
            return ParseExpressionSyntax();
        }
        else if (requestedSyntaxType == typeof(Syntax.BlockSyntax))
        {
            return ParseBlockSyntax();
        }
        else if (requestedSyntaxType == typeof(Syntax.ReturnStatementSyntax))
        {
            return ParseReturnStatementSyntax();
        }
        else if (requestedSyntaxType == typeof(Syntax.NameSyntax))
        {
            return ParseName();
        }
        else if (requestedSyntaxType == typeof(Syntax.IdentifierNameSyntax))
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

    private SyntaxToken PeekToken(int index = 0) => _tokenizer.PeekToken(index);

    private SyntaxToken ReadToken()
    {
        return ReadTokenCore();
    }

    private SyntaxToken ReadTokenCore()
    {
        CurrentToken = _tokenizer.ReadToken();
        Position += CurrentToken.FullWidth;
        return CurrentToken;
    }

    private TextSpan GetStartOfLastToken()
    {
        return new TextSpan(Position - CurrentToken.FullWidth, 0);
    }

    private TextSpan GetEndOfLastToken()
    {
        return new TextSpan(Position - CurrentToken.TrailingTrivia.Count(), 0);
    }

    private void SetPosition(int offset)
    {
        Position = offset;
    }

    private class BlockContext(BlockContext? parent = null)
    {
        public BlockContext? Parent { get; } = parent;
        public List<StatementSyntax> Statements { get; } = [];

        public StatementSyntax? LastStatement
        {
            get
            {
                if (Statements.Any())
                {
                    return Statements.Last();
                }

                return Parent?.LastStatement;
            }
        }

        public void ReplaceStatement(StatementSyntax oldStatement, StatementSyntax newStatement)
        {
            var index = Statements.IndexOf(oldStatement);
            if (index > -1)
            {
                Statements[index] = newStatement;

                return;
            }

            Parent?.ReplaceStatement(oldStatement, newStatement);
        }
    }
}