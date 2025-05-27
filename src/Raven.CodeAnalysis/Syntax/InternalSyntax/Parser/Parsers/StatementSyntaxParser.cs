namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class StatementSyntaxParser : SyntaxParser
{
    public StatementSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    public StatementSyntax ParseStatement()
    {
        var token = PeekToken();

        StatementSyntax? statement;

        switch (token.Kind)
        {
            case SyntaxKind.FunKeyword:
                statement = ParseFunctionSyntax();
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

    private StatementSyntax? ParseFunctionSyntax()
    {
        var funKeyword = ReadToken();

        var name = new NameSyntaxParser(this).ParseSimpleName();

        var parameters = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationSyntaxParser(this).ParseReturnTypeAnnotation();

        var block = new ExpressionSyntaxParser(this).ParseBlockSyntax();

        return LocalFunctionStatement(funKeyword, name, parameters, returnParameterAnnotation, block);
    }

    private ParameterListSyntax ParseParameterList()
    {
        var openParenToken = ReadToken();

        List<GreenNode> parameterList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            var name = new NameSyntaxParser(this).ParseSimpleName();
            var typeAnnotation = new TypeAnnotationSyntaxParser(this).ParseTypeAnnotation();

            parameterList.Add(Parameter(name, typeAnnotation));

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

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

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

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

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

            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ));
        }

        return ExpressionStatementWithSemicolon(expression, semicolonToken, diagnostics);
    }

    public StatementSyntax? LastStatement { get; set; }

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        List<DiagnosticInfo>? diagnostics = null;

        var declaration = ParseVariableDeclarationSyntax();

        if (!ConsumeToken(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            semicolonToken = MissingToken(SyntaxKind.SemicolonToken);

            AddDiagnostic(
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

        var name = new NameSyntaxParser(this).ParseSimpleName();

        EqualsValueClauseSyntax? initializer = null;

        var typeAnnotation = new TypeAnnotationSyntaxParser(this).ParseTypeAnnotation();

        if (IsNextToken(SyntaxKind.EqualsToken, out var _))
        {
            initializer = new EqualsValueClauseSyntaxParser(this).Parse();
        }

        var declarators = new SyntaxList(
            [VariableDeclarator(name, typeAnnotation, initializer)]);

        return new VariableDeclarationSyntax(letOrVarKeyword, declarators);
    }

    private TypeAnnotationSyntax? ParseTypeAnnotationSyntax()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return TypeAnnotation(colonToken, type);
        }

        return null;
    }
}