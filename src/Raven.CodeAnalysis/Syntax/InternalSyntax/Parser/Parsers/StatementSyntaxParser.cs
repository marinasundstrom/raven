namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class StatementSyntaxParser : SyntaxParser
{
    public StatementSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    public StatementSyntax ParseStatement()
    {
        SetTreatNewlinesAsTokens(false); // treat newlines as statement terminators

        var token = PeekToken();

        StatementSyntax? statement;

        switch (token.Kind)
        {
            case SyntaxKind.FuncKeyword:
                statement = ParseLocalFunctionSyntax();
                break;

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

        return statement;
    }

    private StatementSyntax? ParseLocalFunctionSyntax()
    {
        var funcKeyword = ReadToken();

        if (!ConsumeToken(SyntaxKind.IdentifierToken, out var identifier))
        {

        }

        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        var block = new ExpressionSyntaxParser(this).ParseBlockSyntax();

        if (!TryConsumeTerminator(out var terminatorToken))
        {

        }

        return LocalFunctionStatement(funcKeyword, identifier, parameterList, returnParameterAnnotation, block, terminatorToken);
    }

    public ParameterListSyntax ParseParameterList()
    {
        var openParenToken = ReadToken();

        List<GreenNode> parameterList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            SyntaxList modifiers = SyntaxList.Empty;

            SyntaxToken modifier;
            if (ConsumeToken(SyntaxKind.RefKeyword, out modifier) || ConsumeToken(SyntaxKind.OutKeyword, out modifier) || ConsumeToken(SyntaxKind.InKeyword, out modifier))
            {
                modifiers = modifiers.Add(modifier);
            }

            if (!ConsumeToken(SyntaxKind.IdentifierToken, out var name))
            {

            }

            var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

            parameterList.Add(Parameter(modifiers, name, typeAnnotation));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                parameterList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ParameterList(openParenToken, List(parameterList.ToArray()), closeParenToken);
    }

    private StatementSyntax? ParseReturnStatementSyntax()
    {
        var returnKeyword = ReadToken();

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpressionOrNull();

        SetTreatNewlinesAsTokens(true);

        if (!TryConsumeTerminator(out var terminatorToken))
        {
            terminatorToken = SkipUntil(SyntaxKind.NewLineToken, SyntaxKind.SemicolonToken);

            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetFullSpanOfLastToken()));
        }

        return ReturnStatement(returnKeyword, expression, terminatorToken, Diagnostics);
    }

    private StatementSyntax? ParseDeclarationOrExpressionStatementSyntax()
    {
        var token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.LetKeyword:
            case SyntaxKind.VarKeyword:
                return ParseLocalDeclarationStatementSyntax();

                /*
                                case SyntaxKind.IfKeyword:
                                    var ifExpr = ParseIfExpressionSyntax();
                                    return new ExpressionStatementSyntax(ifExpr, diagnostics);

                                case SyntaxKind.WhileKeyword:
                                    var whileExpr = ParseWhileExpressionSyntax();
                                    return new ExpressionStatementSyntax(whileExpr, diagnostics);
                                    */
        }

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        if (expression.IsMissing)
        {
            var unexpectedToken = ReadToken();

            var unexpectedTokenNoTrivia = unexpectedToken
                .WithLeadingTrivia()
                .WithTrailingTrivia();

            var span = GetStartOfLastToken();
            var unexpectedTokenLeadingTriviaWidth = unexpectedToken.LeadingTrivia.Width;

            var trailingTrivia = LastStatement?.GetTrailingTrivia() ?? SyntaxTriviaList.Empty;
            IEnumerable<SyntaxTrivia> trivia = [.. trailingTrivia, .. unexpectedToken.GetLeadingTrivia(), Trivia(SkippedTokensTrivia(TokenList(unexpectedTokenNoTrivia))), .. unexpectedToken.TrailingTrivia];

            if (LastStatement is not null)
            {
                var lastTerminal = LastStatement.GetLastToken();

                var oldLast = LastStatement;
                var lastStatement = (StatementSyntax)LastStatement.ReplaceNode(
                    lastTerminal, lastTerminal.WithTrailingTrivia(trivia));

                //Block.ReplaceStatement(oldLast, lastStatement);
            }

            // INFO: Remember
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.InvalidExpressionTerm,
                    new TextSpan(span.Start + unexpectedTokenLeadingTriviaWidth, span.Length),
                    [unexpectedToken.GetValueText()]
                ));

            if (LastStatement is null)
            {
                return ExpressionStatement(new ExpressionSyntax.Missing(), null, Diagnostics);
            }

            return null;
        }

        if (expression is IfExpressionSyntax or WhileExpressionSyntax or BlockSyntax)
        {
            SetTreatNewlinesAsTokens(true);

            TryConsumeTerminator(out var terminatorToken2);

            return ExpressionStatement(expression, terminatorToken2, Diagnostics);
        }

        SyntaxToken? terminatorToken = ConsumeTerminator();

        return ExpressionStatement(expression, terminatorToken, Diagnostics);
    }

    public StatementSyntax? LastStatement { get; set; }

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        var declaration = ParseVariableDeclarationSyntax();

        SyntaxToken? terminatorToken = ConsumeTerminator();

        return LocalDeclarationStatement(declaration, terminatorToken, Diagnostics);
    }

    private SyntaxToken ConsumeTerminator()
    {
        var terminatorToken = PeekToken();
        if (terminatorToken.IsKind(SyntaxKind.EndOfFileToken))
        {
            return ReadToken();
        }
        else
        {
            SetTreatNewlinesAsTokens(true);

            if (!TryConsumeTerminator(out terminatorToken))
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.SemicolonExpected,
                        GetEndOfLastToken()));
            }
        }

        return terminatorToken;
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var letOrVarKeyword = ReadToken();

        if (!ConsumeToken(SyntaxKind.IdentifierToken, out var identifier))
        {

        }

        EqualsValueClauseSyntax? initializer = null;

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

        if (IsNextToken(SyntaxKind.EqualsToken, out var _))
        {
            initializer = new EqualsValueClauseSyntaxParser(this).Parse();
        }

        var declarators = new SyntaxList(
            [VariableDeclarator(identifier, typeAnnotation, initializer)]);

        return new VariableDeclarationSyntax(letOrVarKeyword, declarators);
    }

    private TypeAnnotationClauseSyntax? ParseTypeAnnotationClauseSyntax()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return TypeAnnotationClause(colonToken, type);
        }

        return null;
    }
}
