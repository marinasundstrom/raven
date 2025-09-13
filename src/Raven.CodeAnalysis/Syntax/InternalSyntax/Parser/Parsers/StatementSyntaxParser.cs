namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

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
                statement = ParseFunctionSyntax();
                break;

            case SyntaxKind.ReturnKeyword:
                statement = ParseReturnStatementSyntax();
                break;

            case SyntaxKind.IfKeyword:
                statement = ParseIfStatementSyntax();
                break;

            case SyntaxKind.OpenBraceToken:
                statement = ParseBlockStatementSyntax();
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

    private IfStatementSyntax ParseIfStatementSyntax()
    {
        var ifKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this).ParseExpression();

        var thenStatement = ParseStatement();

        SyntaxToken? elseKeyword = null;
        StatementSyntax? elseStatement = null;

        if (ConsumeToken(SyntaxKind.ElseKeyword, out var elseTok))
        {
            elseKeyword = elseTok;
            elseStatement = ParseStatement();
        }

        SetTreatNewlinesAsTokens(true);
        TryConsumeTerminator(out var terminatorToken);

        return IfStatement(ifKeyword, condition!, thenStatement!, elseKeyword, elseStatement, terminatorToken);
    }

    private StatementSyntax? ParseFunctionSyntax()
    {
        var funcKeyword = ReadToken();
        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        var block = ParseBlockStatementSyntax();

        TryConsumeTerminator(out var terminatorToken);

        return FunctionStatement(funcKeyword, identifier, parameterList, returnParameterAnnotation, block, terminatorToken);
    }

    public BlockStatementSyntax ParseBlockStatementSyntax()
    {
        var openBrace = ExpectToken(SyntaxKind.OpenBraceToken);

        EnterParens();
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

        return BlockStatement(openBrace, List(statements), closeBrace);
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

            SyntaxToken name;
            if (CanTokenBeIdentifier(PeekToken()))
            {
                name = ReadIdentifierToken();
            }
            else
            {
                name = ExpectToken(SyntaxKind.IdentifierToken);
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

        ExpressionSyntax? expression = null;
        var next = PeekToken();
        if (next.Kind is not (SyntaxKind.SemicolonToken or SyntaxKind.CloseBraceToken or SyntaxKind.EndOfFileToken or SyntaxKind.ElseKeyword))
            expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(true);

        SyntaxToken? terminatorToken = null;
        if (PeekToken().Kind != SyntaxKind.ElseKeyword)
        {
            if (!TryConsumeTerminator(out terminatorToken))
            {
                SkipUntil(SyntaxKind.NewLineToken, SyntaxKind.SemicolonToken);
            }
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
        }

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        if (expression.IsMissing)
        {
            var terminatorToken2 = ConsumeTerminator();

            return EmptyStatement(terminatorToken2);
        }

        if (expression is WhileExpressionSyntax or ForExpressionSyntax)
        {
            SetTreatNewlinesAsTokens(true);

            TryConsumeTerminator(out var terminatorToken2);

            return ExpressionStatement(expression, terminatorToken2, Diagnostics);
        }

        if (expression is AssignmentExpressionSyntax assignment)
        {
            var assignmentTerminatorToken = ConsumeTerminator();
            var kind = assignment.Kind switch
            {
                SyntaxKind.SimpleAssignmentExpression => SyntaxKind.SimpleAssignmentStatement,
                _ => SyntaxKind.SimpleAssignmentStatement,
            };

            return AssignmentStatement(kind, assignment.Left, assignment.OperatorToken, assignment.Right, assignmentTerminatorToken, Diagnostics);
        }

        var terminatorToken = ConsumeTerminator();

        return ExpressionStatement(expression, terminatorToken, Diagnostics);
    }

    public StatementSyntax? LastStatement { get; set; }

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        var declaration = ParseVariableDeclarationSyntax();

        var terminatorToken = ConsumeTerminator();

        return LocalDeclarationStatement(declaration, terminatorToken, Diagnostics);
    }

    private SyntaxToken ConsumeTerminator()
    {
        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        return terminatorToken;
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var letOrVarKeyword = ReadToken();

        SyntaxToken identifier = MissingToken(SyntaxKind.IdentifierToken);

        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
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
