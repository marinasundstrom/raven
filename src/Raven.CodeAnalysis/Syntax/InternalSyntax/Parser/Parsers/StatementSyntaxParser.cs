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

        if (IsPossibleLabeledStatementStart(token))
        {
            statement = ParseLabeledStatementSyntax();
        }
        else
        {
            switch (token.Kind)
            {
                case SyntaxKind.FuncKeyword:
                case SyntaxKind.AsyncKeyword when PeekToken(1).Kind == SyntaxKind.FuncKeyword:
                    statement = ParseFunctionSyntax();
                    break;

                case SyntaxKind.YieldKeyword:
                    statement = ParseYieldStatementSyntax();
                    break;

                case SyntaxKind.ReturnKeyword:
                    statement = ParseReturnStatementSyntax();
                    break;

                case SyntaxKind.ThrowKeyword:
                    statement = ParseThrowStatementSyntax();
                    break;

                case SyntaxKind.IfKeyword:
                    statement = ParseIfStatementSyntax();
                    break;

                case SyntaxKind.WhileKeyword:
                    statement = ParseWhileStatementSyntax();
                    break;

                case SyntaxKind.UsingKeyword:
                    statement = ParseUsingDeclarationStatementSyntax();
                    break;

                case SyntaxKind.TryKeyword:
                    statement = ParseTryStatementSyntax();
                    break;

                case SyntaxKind.ForKeyword:
                    statement = ParseForStatementSyntax();
                    break;

                case SyntaxKind.OpenBraceToken:
                    statement = ParseBlockStatementSyntax();
                    break;

                case SyntaxKind.GotoKeyword:
                    statement = ParseGotoStatementSyntax();
                    break;

                case SyntaxKind.BreakKeyword:
                    statement = ParseBreakStatementSyntax();
                    break;

                case SyntaxKind.ContinueKeyword:
                    statement = ParseContinueStatementSyntax();
                    break;

                case SyntaxKind.SemicolonToken:
                    ReadToken();
                    statement = EmptyStatement(token);
                    break;

                default:
                    statement = ParseDeclarationOrExpressionStatementSyntax();
                    break;
            }
        }

        return statement;
    }

    private bool IsPossibleLabeledStatementStart(SyntaxToken token)
    {
        if (!CanTokenBeIdentifier(token))
        {
            if (!global::Raven.CodeAnalysis.Syntax.SyntaxFacts.IsReservedWordKind(token.Kind))
            {
                return false;
            }
        }

        var colonCandidate = PeekToken(1);
        return colonCandidate.Kind == SyntaxKind.ColonToken;
    }

    private LabeledStatementSyntax ParseLabeledStatementSyntax()
    {
        SyntaxToken identifier;
        if (global::Raven.CodeAnalysis.Syntax.SyntaxFacts.IsReservedWordKind(PeekToken().Kind))
        {
            identifier = ReadToken();
        }
        else
        {
            identifier = ReadIdentifierToken();
        }
        var colonToken = ExpectToken(SyntaxKind.ColonToken);

        StatementSyntax statement;
        if (IsTokenPotentialStatementStart(PeekToken()))
        {
            statement = ParseStatement();
        }
        else
        {
            var terminator = Token(SyntaxKind.None);
            statement = EmptyStatement(terminator);
        }

        return LabeledStatement(identifier, colonToken, statement);
    }

    private static bool IsTokenPotentialStatementStart(SyntaxToken token)
    {
        return token.Kind switch
        {
            SyntaxKind.None => false,
            SyntaxKind.EndOfFileToken => false,
            SyntaxKind.CloseBraceToken => false,
            SyntaxKind.CloseParenToken => false,
            SyntaxKind.CloseBracketToken => false,
            SyntaxKind.SemicolonToken => true,
            SyntaxKind.CommaToken => false,
            SyntaxKind.ColonToken => false,
            SyntaxKind.NewLineToken => false,
            SyntaxKind.LineFeedToken => false,
            SyntaxKind.CarriageReturnToken => false,
            SyntaxKind.CarriageReturnLineFeedToken => false,
            _ => true,
        };
    }

    private GotoStatementSyntax ParseGotoStatementSyntax()
    {
        var gotoKeyword = ReadToken();

        SyntaxToken identifier;
        var next = PeekToken();
        if (CanTokenBeIdentifier(next))
        {
            identifier = ReadIdentifierToken();
        }
        else if (global::Raven.CodeAnalysis.Syntax.SyntaxFacts.IsReservedWordKind(next.Kind))
        {
            identifier = ReadToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        var terminatorToken = ConsumeTerminator();

        return GotoStatement(gotoKeyword, identifier, terminatorToken, Diagnostics);
    }

    private BreakStatementSyntax ParseBreakStatementSyntax()
    {
        var breakKeyword = ReadToken();

        var terminatorToken = ConsumeTerminator();

        return BreakStatement(breakKeyword, terminatorToken, Diagnostics);
    }

    private ContinueStatementSyntax ParseContinueStatementSyntax()
    {
        var continueKeyword = ReadToken();

        var terminatorToken = ConsumeTerminator();

        return ContinueStatement(continueKeyword, terminatorToken, Diagnostics);
    }

    private StatementSyntax ParseYieldStatementSyntax()
    {
        var yieldKeyword = ReadToken();

        var next = PeekToken();

        if (next.Kind == SyntaxKind.BreakKeyword)
        {
            return ParseYieldBreakStatementSyntax(yieldKeyword);
        }

        var returnKeyword = next.Kind == SyntaxKind.ReturnKeyword
            ? ReadToken()
            : ExpectToken(SyntaxKind.ReturnKeyword);

        return ParseYieldReturnStatementSyntax(yieldKeyword, returnKeyword);
    }

    private YieldReturnStatementSyntax ParseYieldReturnStatementSyntax(SyntaxToken yieldKeyword, SyntaxToken returnKeyword)
    {
        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(true);

        SyntaxToken? terminatorToken = null;
        if (PeekToken().Kind != SyntaxKind.ElseKeyword)
        {
            if (!TryConsumeTerminator(out terminatorToken))
            {
                SkipUntil(SyntaxKind.NewLineToken, SyntaxKind.SemicolonToken);
            }
        }
        else
        {
            terminatorToken = Token(SyntaxKind.None);
        }

        return YieldReturnStatement(yieldKeyword, returnKeyword, expression, terminatorToken, Diagnostics);
    }

    private YieldBreakStatementSyntax ParseYieldBreakStatementSyntax(SyntaxToken yieldKeyword)
    {
        var breakKeyword = ExpectToken(SyntaxKind.BreakKeyword);

        var terminatorToken = ConsumeTerminator();

        return YieldBreakStatement(yieldKeyword, breakKeyword, terminatorToken, Diagnostics);
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

    private WhileStatementSyntax ParseWhileStatementSyntax()
    {
        var whileKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this).ParseExpression();
        var statement = ParseStatement();

        SetTreatNewlinesAsTokens(true);
        TryConsumeTerminator(out var terminatorToken);

        return WhileStatement(whileKeyword, condition!, statement!, terminatorToken);
    }

    private TryStatementSyntax ParseTryStatementSyntax()
    {
        var tryKeyword = ReadToken();

        var block = ParseBlockStatementSyntax();

        var catchClauses = new List<CatchClauseSyntax>();

        while (IsNextToken(SyntaxKind.CatchKeyword, out _))
        {
            catchClauses.Add(ParseCatchClauseSyntax());
        }

        FinallyClauseSyntax? finallyClause = null;

        if (IsNextToken(SyntaxKind.FinallyKeyword, out _))
        {
            finallyClause = ParseFinallyClauseSyntax();
        }

        if (catchClauses.Count == 0 && finallyClause is null)
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.TryStatementRequiresCatchOrFinally,
                GetSpanOfLastToken()));
        }

        SetTreatNewlinesAsTokens(true);
        TryConsumeTerminator(out var terminatorToken);

        return TryStatement(
            tryKeyword,
            block,
            List(catchClauses.ToArray()),
            finallyClause,
            terminatorToken,
            Diagnostics);
    }

    private CatchClauseSyntax ParseCatchClauseSyntax()
    {
        var catchKeyword = ReadToken();

        CatchDeclarationSyntax? declaration = null;

        if (ConsumeToken(SyntaxKind.OpenParenToken, out var openParenToken))
        {
            var type = new NameSyntaxParser(this).ParseTypeName();

            SyntaxToken? identifier = null;

            if (CanTokenBeIdentifier(PeekToken()))
            {
                identifier = ReadIdentifierToken();
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

            declaration = CatchDeclaration(openParenToken, type!, identifier, closeParenToken);
        }

        var block = ParseBlockStatementSyntax();

        return CatchClause(catchKeyword, declaration, block);
    }

    private FinallyClauseSyntax ParseFinallyClauseSyntax()
    {
        var finallyKeyword = ReadToken();
        var block = ParseBlockStatementSyntax();
        return FinallyClause(finallyKeyword, block);
    }

    private ForStatementSyntax ParseForStatementSyntax()
    {
        var forKeyword = ReadToken();

        SyntaxToken eachKeyword;
        if (IsNextToken(SyntaxKind.EachKeyword, out _))
            eachKeyword = ReadToken();
        else
            eachKeyword = Token(SyntaxKind.None);

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        ConsumeTokenOrMissing(SyntaxKind.InKeyword, out var inKeyword);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();
        var body = ParseStatement();

        SetTreatNewlinesAsTokens(true);
        TryConsumeTerminator(out var terminatorToken);

        return ForStatement(forKeyword, eachKeyword, identifier, inKeyword, expression!, body!, terminatorToken);
    }

    private StatementSyntax? ParseFunctionSyntax()
    {
        var modifiers = ParseFunctionModifiers();

        var funcKeyword = ExpectToken(SyntaxKind.FuncKeyword);
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

        return FunctionStatement(modifiers, funcKeyword, identifier, parameterList, returnParameterAnnotation, block, terminatorToken);
    }

    private SyntaxList ParseFunctionModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.AsyncKeyword)
            {
                modifiers = modifiers.Add(ReadToken());
            }
            else
            {
                break;
            }
        }

        return modifiers;
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

            var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

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
                name = MissingToken(SyntaxKind.IdentifierToken);

                if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
                {
                    ReadToken();
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetSpanOfLastToken()));
                }
                else
                {
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetEndOfLastToken()));
                }
            }

            var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

            EqualsValueClauseSyntax? defaultValue = null;
            if (IsNextToken(SyntaxKind.EqualsToken, out _))
            {
                defaultValue = new EqualsValueClauseSyntaxParser(this).Parse();
            }

            parameterList.Add(Parameter(attributeLists, modifiers, name, typeAnnotation, defaultValue));

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
        else
        {
            // When the next token is an 'else', the return statement has no
            // explicit terminator. Use a placeholder so downstream consumers
            // don't encounter a null token.
            terminatorToken = Token(SyntaxKind.None);
        }

        return ReturnStatement(returnKeyword, expression, terminatorToken, Diagnostics);
    }

    private StatementSyntax ParseThrowStatementSyntax()
    {
        var throwKeyword = ReadToken();

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(true);

        var terminatorToken = ConsumeTerminator();

        return ThrowStatement(throwKeyword, expression, terminatorToken, Diagnostics);
    }

    private UsingDeclarationStatementSyntax ParseUsingDeclarationStatementSyntax()
    {
        var usingKeyword = ReadToken();
        var declaration = ParseVariableDeclarationSyntax();
        var terminatorToken = ConsumeTerminator();

        return UsingDeclarationStatement(usingKeyword, declaration, terminatorToken, Diagnostics);
    }

    private StatementSyntax? ParseDeclarationOrExpressionStatementSyntax()
    {
        var token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.LetKeyword:
            case SyntaxKind.VarKeyword:
                if (PeekToken(1).Kind != SyntaxKind.OpenParenToken)
                    return ParseLocalDeclarationStatementSyntax();
                break;
        }

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        if (expression.IsMissing)
        {
            var terminatorToken2 = ConsumeTerminator();

            return EmptyStatement(terminatorToken2);
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
