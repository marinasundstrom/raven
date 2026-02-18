namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;
using System.Linq;

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

        if (!IsTokenPotentialStatementStart(token))
        {
            return ParseIncompleteStatement();
        }

        if (IsPossibleLabeledStatementStart(token))
        {
            statement = ParseLabeledStatementSyntax();
        }
        else if (token.Kind == SyntaxKind.OpenBracketToken && TryParseAttributedFunctionSyntax(out var attributedFunction))
        {
            statement = attributedFunction;
        }
        else
        {
            switch (token.Kind)
            {
                case SyntaxKind.FuncKeyword:
                case SyntaxKind.AsyncKeyword when PeekToken(1).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.ExternKeyword when PeekToken(1).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.ExternKeyword when PeekToken(1).Kind == SyntaxKind.AsyncKeyword && PeekToken(2).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.ExternKeyword when PeekToken(1).Kind == SyntaxKind.UnsafeKeyword && PeekToken(2).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.ExternKeyword when PeekToken(1).Kind == SyntaxKind.UnsafeKeyword && PeekToken(2).Kind == SyntaxKind.AsyncKeyword && PeekToken(3).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.AsyncKeyword when PeekToken(1).Kind == SyntaxKind.ExternKeyword && PeekToken(2).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.UnsafeKeyword when PeekToken(1).Kind == SyntaxKind.ExternKeyword && PeekToken(2).Kind == SyntaxKind.FuncKeyword:
                case SyntaxKind.UnsafeKeyword when PeekToken(1).Kind == SyntaxKind.AsyncKeyword && PeekToken(2).Kind == SyntaxKind.ExternKeyword && PeekToken(3).Kind == SyntaxKind.FuncKeyword:
                    statement = ParseFunctionSyntax(SyntaxList.Empty, SyntaxList.Empty);
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

                case SyntaxKind.UseKeyword:
                    statement = ParseUseDeclarationStatementSyntax();
                    break;

                case SyntaxKind.TryKeyword:
                    var next = PeekToken(1);
                    if (!next.IsKind(SyntaxKind.OpenBraceToken))
                    {
                        return ParseDeclarationOrExpressionStatementSyntax()!;
                    }
                    statement = ParseTryStatementSyntax();
                    break;

                case SyntaxKind.ForKeyword:
                    statement = ParseForStatementSyntax();
                    break;

                case SyntaxKind.OpenBraceToken:
                    statement = ParseBlockStatementSyntax();
                    break;

                case SyntaxKind.UnsafeKeyword:
                    if (PeekToken(1).Kind == SyntaxKind.FuncKeyword ||
                        (PeekToken(1).Kind == SyntaxKind.AsyncKeyword && PeekToken(2).Kind == SyntaxKind.FuncKeyword) ||
                        (PeekToken(1).Kind == SyntaxKind.ExternKeyword && PeekToken(2).Kind == SyntaxKind.FuncKeyword) ||
                        (PeekToken(1).Kind == SyntaxKind.ExternKeyword && PeekToken(2).Kind == SyntaxKind.AsyncKeyword && PeekToken(3).Kind == SyntaxKind.FuncKeyword))
                    {
                        statement = ParseFunctionSyntax(SyntaxList.Empty, SyntaxList.Empty);
                    }
                    else
                    {
                        statement = ParseUnsafeStatementSyntax();
                    }
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

                case SyntaxKind.MatchKeyword:
                    statement = ParseMatchStatementSyntax();
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

    private bool TryParseAttributedFunctionSyntax(out StatementSyntax? functionStatement)
    {
        functionStatement = null;

        var checkpoint = CreateCheckpoint("attributed-function-statement");
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseFunctionModifiers();

        if (!PeekToken().IsKind(SyntaxKind.FuncKeyword))
        {
            checkpoint.Rewind();
            return false;
        }

        functionStatement = ParseFunctionSyntax(attributeLists, modifiers);
        return true;
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

    internal static bool IsTokenPotentialStatementStart(SyntaxToken token)
    {
        return token.Kind switch
        {
            SyntaxKind.None => false,
            SyntaxKind.EndOfFileToken => false,
            SyntaxKind.CloseBraceToken => false,
            SyntaxKind.CloseParenToken => false,
            SyntaxKind.CloseBracketToken => false,
            SyntaxKind.ArrowToken => false,
            SyntaxKind.FatArrowToken => false,
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

    private IncompleteStatementSyntax ParseIncompleteStatement()
    {
        var peek = PeekToken();
        var span = GetSpanOfPeekedToken();

        var skippedTokens = ConsumeSkippedTokensUntil(static token =>
            token.Kind is SyntaxKind.SemicolonToken or SyntaxKind.CloseBraceToken or SyntaxKind.NewLineToken or
            SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken ||
            IsTokenPotentialStatementStart(token));

        var skippedToken = CreateSkippedToken(skippedTokens, span);

        return IncompleteStatement(skippedToken);
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

        return GotoStatement(gotoKeyword, identifier, terminatorToken);
    }

    private BreakStatementSyntax ParseBreakStatementSyntax()
    {
        var breakKeyword = ReadToken();

        var terminatorToken = ConsumeTerminator();

        return BreakStatement(breakKeyword, terminatorToken);
    }

    private ContinueStatementSyntax ParseContinueStatementSyntax()
    {
        var continueKeyword = ReadToken();

        var terminatorToken = ConsumeTerminator();

        return ContinueStatement(continueKeyword, terminatorToken);
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

        return YieldReturnStatement(yieldKeyword, returnKeyword, expression, terminatorToken);
    }

    private YieldBreakStatementSyntax ParseYieldBreakStatementSyntax(SyntaxToken yieldKeyword)
    {
        var breakKeyword = ExpectToken(SyntaxKind.BreakKeyword);

        var terminatorToken = ConsumeTerminator();

        return YieldBreakStatement(yieldKeyword, breakKeyword, terminatorToken);
    }

    private IfStatementSyntax ParseIfStatementSyntax()
    {
        var ifKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this, stopOnOpenBrace: true).ParseExpression();

        var p = PeekToken();

        if (!p.IsKind(SyntaxKind.OpenBraceToken))
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetEndOfLastToken(1), '{'));
        }

        var thenStatement = ParseStatement();

        SyntaxToken? elseKeyword = null;
        StatementSyntax? elseStatement = null;

        ElseClause2Syntax? elseClause = null;

        if (ConsumeToken(SyntaxKind.ElseKeyword, out var elseTok))
        {
            elseKeyword = elseTok;

            p = PeekToken();

            if (!p.IsKind(SyntaxKind.OpenBraceToken) && !p.IsKind(SyntaxKind.IfKeyword))
            {
                AddDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetEndOfLastToken(1), '{'));
            }

            elseStatement = ParseStatement();

            elseClause = ElseClause2(elseKeyword, elseStatement);
        }

        SetTreatNewlinesAsTokens(true);
        TryConsumeTerminator(out var terminatorToken);

        return IfStatement(ifKeyword, condition!, thenStatement!, elseClause, terminatorToken);
    }

    private WhileStatementSyntax ParseWhileStatementSyntax()
    {
        var whileKeyword = ReadToken();

        var condition = new ExpressionSyntaxParser(this, stopOnOpenBrace: true).ParseExpression();
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
            terminatorToken);
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
        var current = PeekToken();
        if (current.Kind is SyntaxKind.InKeyword)
        {
            identifier = Token(SyntaxKind.None);
        }
        else if (current.Kind is SyntaxKind.UnderscoreToken)
        {
            identifier = ReadToken();
        }
        else if (CanTokenBeIdentifier(current))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        ConsumeTokenOrMissing(SyntaxKind.InKeyword, out var inKeyword);

        var expression = new ExpressionSyntaxParser(this, stopOnOpenBrace: true).ParseExpression();
        var body = ParseStatement();

        SetTreatNewlinesAsTokens(true);
        TryConsumeTerminator(out var terminatorToken);

        return ForStatement(forKeyword, eachKeyword, identifier, inKeyword, expression!, body!, terminatorToken);
    }

    private StatementSyntax? ParseFunctionSyntax(SyntaxList attributeLists, SyntaxList modifiers)
    {
        modifiers = !modifiers.GetChildren().Any() ? ParseFunctionModifiers() : modifiers;
        var isExtern = modifiers.GetChildren().Any(child => child.IsKind(SyntaxKind.ExternKeyword));

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

        TypeParameterListSyntax? typeParameterList = null;
        if (IsNextToken(SyntaxKind.LessThanToken, out _))
        {
            var typeParameterParser = new TypeDeclarationParser(this);
            typeParameterList = typeParameterParser.ParseTypeParameterList();
        }

        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();

        BlockStatementSyntax? block = null;
        ArrowExpressionClauseSyntax? expressionBody = null;

        if (IsNextToken(SyntaxKind.OpenBraceToken, out _))
        {
            block = ParseBlockStatementSyntax();
        }
        else if (IsNextToken(SyntaxKind.FatArrowToken, out _))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }
        else if (!isExtern)
        {
            block = ParseBlockStatementSyntax();
        }

        TryConsumeTerminator(out var terminatorToken);

        return FunctionStatement(
            attributeLists,
            modifiers,
            funcKeyword,
            identifier,
            typeParameterList,
            parameterList,
            returnParameterAnnotation,
            constraintClauses,
            block,
            expressionBody,
            terminatorToken);
    }

    private SyntaxList ParseFunctionModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.AsyncKeyword or SyntaxKind.UnsafeKeyword or SyntaxKind.ExternKeyword)
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

    private UnsafeStatementSyntax ParseUnsafeStatementSyntax()
    {
        var unsafeKeyword = ReadToken();
        var block = ParseBlockStatementSyntax();
        return UnsafeStatement(unsafeKeyword, block);
    }

    public BlockStatementSyntax ParseBlockStatementSyntax()
    {
        var openBrace = ExpectToken(SyntaxKind.OpenBraceToken);

        EnterParens();
        var statements = new List<StatementSyntax>();

        while (!IsNextToken(SyntaxKind.CloseBraceToken, out _) &&
               !IsNextToken(SyntaxKind.EndOfFileToken, out _))
        {
            var statementStart = Position;
            var stmt = new StatementSyntaxParser(this).ParseStatement();
            if (stmt is not null)
                statements.Add(stmt);

            if (Position == statementStart)
            {
                var token = PeekToken();
                var tokenText = string.IsNullOrEmpty(token.Text)
                    ? token.Kind.ToString()
                    : token.Text;

                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                        GetSpanOfPeekedToken(),
                        tokenText));

                ReadToken();
            }

            SetTreatNewlinesAsTokens(false);
        }

        ExitParens();

        var closeBrace = ExpectToken(SyntaxKind.CloseBraceToken);

        return BlockStatement(openBrace, List(statements), closeBrace);
    }

    public ParameterListSyntax ParseParameterList(SyntaxToken? openParenToken = null)
    {
        var openParenTokenValue = openParenToken ?? ReadToken();

        List<GreenNode> parameterList = new List<GreenNode>();

        var parsedParameters = 0;
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        try
        {
            while (true)
            {
                var t = PeekToken();

                if (t.IsKind(SyntaxKind.EndOfFileToken) ||
                    t.IsKind(SyntaxKind.CloseParenToken))
                {
                    break;
                }

                if (parsedParameters > 0)
                {
                    if (t.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        parameterList.Add(commaToken);
                    }
                    else
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ","));
                    }
                }

                var parameterStart = Position;
                var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

                SyntaxToken? refKindKeyword = null;
                if (PeekToken().Kind is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword or SyntaxKind.InKeyword)
                    refKindKeyword = ReadToken();

                SyntaxToken? bindingKeyword = null;
                if (PeekToken().Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword or SyntaxKind.ConstKeyword)
                    bindingKeyword = ReadToken();

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
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.IdentifierExpected,
                                GetSpanOfPeekedToken()));
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

                if (Position == parameterStart)
                {
                    var token = PeekToken();
                    var tokenText = string.IsNullOrEmpty(token.Text)
                        ? token.Kind.ToString()
                        : token.Text;

                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                            GetSpanOfPeekedToken(),
                            tokenText));

                    if (token.IsKind(SyntaxKind.CloseParenToken) || token.IsKind(SyntaxKind.EndOfFileToken))
                        break;

                    ReadToken();
                    continue;
                }

                parameterList.Add(Parameter(attributeLists, refKindKeyword, bindingKeyword, name, typeAnnotation, defaultValue));
                parsedParameters++;
            }
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        if (closeParenToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    ")"));
        }

        return ParameterList(openParenTokenValue, List(parameterList.ToArray()), closeParenToken);
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

        return ReturnStatement(returnKeyword, expression, terminatorToken);
    }

    private StatementSyntax ParseThrowStatementSyntax()
    {
        var throwKeyword = ReadToken();

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(true);

        var terminatorToken = ConsumeTerminator();

        return ThrowStatement(throwKeyword, expression, terminatorToken);
    }

    private UseDeclarationStatementSyntax ParseUseDeclarationStatementSyntax()
    {
        var useKeyword = ReadToken();
        var declaration = ParseVariableDeclarationSyntax(isBindingKeywordOptional: true);
        if (!(declaration.BindingKeyword.IsKind(SyntaxKind.ValKeyword) || declaration.BindingKeyword.IsKind(SyntaxKind.LetKeyword))
            && !declaration.BindingKeyword.IsKind(SyntaxKind.None))
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                CompilerDiagnostics.IdentifierExpected,
                GetEndOfLastToken()));
        }
        var terminatorToken = ConsumeTerminator();

        return UseDeclarationStatement(useKeyword, declaration, terminatorToken);
    }

    private StatementSyntax? ParseDeclarationOrExpressionStatementSyntax()
    {
        var token = PeekToken();

        switch (token.Kind)
        {
            case SyntaxKind.LetKeyword:
            case SyntaxKind.ValKeyword:
            case SyntaxKind.VarKeyword:
            case SyntaxKind.ConstKeyword:
                if (PeekToken(1).Kind != SyntaxKind.OpenParenToken)
                {
                    var declaration = ParseVariableDeclarationSyntax();
                    var declarationTerminator = ConsumeTerminatorWithSkippedTokens(addSemicolonDiagnostic: true);

                    return LocalDeclarationStatement(declaration, declarationTerminator);
                }
                break;
        }

        SetTreatNewlinesAsTokens(false);

        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        if (expression.IsMissing)
        {
            var terminatorToken2 = ConsumeTerminator();

            return EmptyStatement(terminatorToken2);
        }

        var terminatorToken = ConsumeTerminatorWithSkippedTokens(addSemicolonDiagnostic: true);

        if (expression is AssignmentExpressionSyntax assignment)
        {
            var kind = GetAssignmentStatementKind(assignment.Kind, assignment.Left);

            return AssignmentStatement(kind, assignment.Left, assignment.OperatorToken, assignment.Right, terminatorToken);
        }

        return ExpressionStatement(expression, terminatorToken);
    }

    private StatementSyntax ParseMatchStatementSyntax()
    {
        SetTreatNewlinesAsTokens(false);
        var matchExpression = new ExpressionSyntaxParser(this).ParseMatchExpressionStatementForm();
        var terminatorToken = ConsumeTerminatorWithSkippedTokens(addSemicolonDiagnostic: true);
        return MatchStatement(
            matchExpression.MatchKeyword,
            matchExpression.Expression,
            matchExpression.OpenBraceToken,
            matchExpression.Arms,
            matchExpression.CloseBraceToken,
            terminatorToken);
    }

    public StatementSyntax? LastStatement { get; set; }

    private static SyntaxKind GetAssignmentStatementKind(SyntaxKind expressionKind, ExpressionOrPatternSyntax left)
    {
        var isDiscard = left is DiscardPatternSyntax or DiscardExpressionSyntax;

        return expressionKind switch
        {
            SyntaxKind.SimpleAssignmentExpression when isDiscard => SyntaxKind.DiscardAssignmentStatement,
            SyntaxKind.SimpleAssignmentExpression => SyntaxKind.SimpleAssignmentStatement,
            SyntaxKind.AddAssignmentExpression => SyntaxKind.AddAssignmentStatement,
            SyntaxKind.SubtractAssignmentExpression => SyntaxKind.SubtractAssignmentStatement,
            SyntaxKind.MultiplyAssignmentExpression => SyntaxKind.MultiplyAssignmentStatement,
            SyntaxKind.DivideAssignmentExpression => SyntaxKind.DivideAssignmentStatement,
            SyntaxKind.BitwiseAndAssignmentExpression => SyntaxKind.BitwiseAndAssignmentStatement,
            SyntaxKind.BitwiseOrAssignmentExpression => SyntaxKind.BitwiseOrAssignmentStatement,
            SyntaxKind.BitwiseXorAssignmentExpression => SyntaxKind.BitwiseXorAssignmentStatement,
            SyntaxKind.NullCoalesceAssignmentExpression => SyntaxKind.NullCoalesceAssignmentStatement,
            _ => SyntaxKind.SimpleAssignmentStatement,
        };
    }

    private LocalDeclarationStatementSyntax ParseLocalDeclarationStatementSyntax()
    {
        var declaration = ParseVariableDeclarationSyntax();

        var terminatorToken = ConsumeTerminatorWithSkippedTokens(addSemicolonDiagnostic: true);

        return LocalDeclarationStatement(declaration, terminatorToken);
    }

    private SyntaxToken ConsumeTerminatorWithSkippedTokens(bool addSemicolonDiagnostic)
    {
        bool previous = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        var skippedTokens = new List<SyntaxToken>();
        bool reportedDiagnostic = false;

        while (true)
        {
            var current = PeekToken();

            if (IsNewLineToken(current))
            {
                var terminator = ReadToken();
                var tokenWithTrivia = AttachSkippedTokens(terminator, skippedTokens);
                SetTreatNewlinesAsTokens(previous);
                return tokenWithTrivia;
            }

            if (current.Kind == SyntaxKind.SemicolonToken)
            {
                var terminator = ReadToken();
                var tokenWithTrivia = AttachSkippedTokens(terminator, skippedTokens);
                SetTreatNewlinesAsTokens(previous);
                return tokenWithTrivia;
            }

            if (current.Kind == SyntaxKind.EndOfFileToken || current.Kind == SyntaxKind.CloseBraceToken)
            {
                AddSkippedTokensToPending(skippedTokens);

                if (addSemicolonDiagnostic && skippedTokens.Count > 0 && !reportedDiagnostic)
                {
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.ConsecutiveStatementsMustBeSeparatedBySemicolon,
                            GetEndOfLastToken()));
                }

                SetTreatNewlinesAsTokens(previous);
                return Token(SyntaxKind.None);
            }

            if (addSemicolonDiagnostic && !reportedDiagnostic)
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.ConsecutiveStatementsMustBeSeparatedBySemicolon,
                        GetInsertionSpanBeforePeekedToken()));
                reportedDiagnostic = true;
            }

            skippedTokens.Add(ReadToken());
        }

        static bool IsNewLineToken(SyntaxToken token)
        {
            return token.Kind is
                SyntaxKind.LineFeedToken or
                SyntaxKind.CarriageReturnToken or
                SyntaxKind.CarriageReturnLineFeedToken or
                SyntaxKind.NewLineToken;
        }

        SyntaxToken AttachSkippedTokens(SyntaxToken terminator, List<SyntaxToken> skippedTokens)
        {
            if (skippedTokens.Count == 0)
                return terminator;

            var trivia = new SyntaxTrivia(
                new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray()))
            );

            var leadingTrivia = terminator.LeadingTrivia.Add(trivia);
            var newToken = new SyntaxToken(
                terminator.Kind,
                terminator.Text,
                terminator.GetValue(),
                terminator.Width,
                leadingTrivia,
                terminator.TrailingTrivia);

            UpdateLastToken(newToken);

            return newToken;
        }

        void AddSkippedTokensToPending(List<SyntaxToken> skippedTokens)
        {
            if (skippedTokens.Count == 0)
                return;

            var trivia = new SyntaxTrivia(
                new SkippedTokensTrivia(new SyntaxList(skippedTokens.ToArray()))
            );

            GetBaseContext()._pendingTrivia.Add(trivia);
        }
    }

    private SyntaxToken ConsumeTerminator()
    {
        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        return terminatorToken;
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax(bool isBindingKeywordOptional = false)
    {
        var t0 = PeekToken();

        // Explicit binding keyword
        if (t0.IsKind(SyntaxKind.ValKeyword) ||
            t0.IsKind(SyntaxKind.VarKeyword) ||
            t0.IsKind(SyntaxKind.LetKeyword) ||
            t0.IsKind(SyntaxKind.ConstKeyword))
        {
            var bindingKeyword = ReadToken();
            return FinishParseVariableDeclarationSyntax(bindingKeyword);
        }

        // No explicit keyword
        if (!isBindingKeywordOptional)
            return null;

        var syntheticBindingKeyword = Token(SyntaxKind.None);

        return FinishParseVariableDeclarationSyntax(syntheticBindingKeyword);
    }

    private VariableDeclarationSyntax FinishParseVariableDeclarationSyntax(SyntaxToken bindingKeyword)
    {
        SyntaxToken identifier = MissingToken(SyntaxKind.IdentifierToken);

        var next = PeekToken();

        if (next.Kind == SyntaxKind.UnderscoreToken)
        {
            var underscore = ReadToken();
            identifier = ToIdentifierToken(underscore);
            UpdateLastToken(identifier);
        }
        else if (CanTokenBeIdentifier(next))
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

        return new VariableDeclarationSyntax(bindingKeyword, declarators);
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
