namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal sealed class MacroInvocationSyntaxParser : SyntaxParser
{
    public MacroInvocationSyntaxParser(ParseContext parent) : base(parent)
    {
    }

    public bool IsBangInvocationStart(bool allowExpressionHeader = false)
    {
        if (!CanStartMacroName())
            return false;

        var checkpoint = CreateCheckpoint("freestanding-macro-lookahead");
        var name = ParseMacroName();
        var isStart = !name.IsMissing &&
            !HasLineBreakBeforePeekToken() &&
            ConsumeToken(SyntaxKind.ExclamationToken, out _);

        if (!isStart)
        {
            checkpoint.Rewind();
            return false;
        }

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            _ = new ExpressionSyntaxParser(this).ParseArgumentListSyntax(
                allowLegacyNamedArgumentEquals: false);
            checkpoint.Rewind();
            return true;
        }

        if (HasLineBreakBeforePeekToken())
        {
            checkpoint.Rewind();
            return false;
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            checkpoint.Rewind();
            return true;
        }

        if (!allowExpressionHeader || IsOperatorLeadingHeader(PeekToken()))
        {
            checkpoint.Rewind();
            return false;
        }

        var expressionStart = Position;
        _ = new ExpressionSyntaxParser(this, stopOnOpenBrace: true).ParseExpression();
        isStart = Position > expressionStart;
        checkpoint.Rewind();
        return isStart;
    }

    public bool IsDeclarationInvocationStart()
    {
        if (!CanStartMacroName())
            return false;

        var checkpoint = CreateCheckpoint("declaration-macro-lookahead");
        var name = ParseMacroName();
        var isStart = !name.IsMissing &&
            !HasLineBreakBeforePeekToken() &&
            ConsumeToken(SyntaxKind.ExclamationToken, out _) &&
            !HasLineBreakBeforePeekToken() &&
            CanTokenBeIdentifier(PeekToken());

        if (isStart)
        {
            _ = ReadIdentifierToken();
            isStart = PeekToken().IsKind(SyntaxKind.OpenParenToken) ||
                PeekToken().IsKind(SyntaxKind.OpenBraceToken);
        }

        checkpoint.Rewind();
        return isStart;
    }

    public FreestandingMacroExpressionSyntax ParseExpression()
    {
        var invocation = ParseInvocation(allowExpressionHeader: true);
        return FreestandingMacroExpression(
            invocation.Name,
            invocation.ExclamationToken,
            invocation.Carrier);
    }

    public FreestandingMacroMemberDeclarationSyntax ParseMember(
        SyntaxList attributeLists,
        SyntaxList modifiers)
    {
        var invocation = ParseInvocation(allowExpressionHeader: false);
        TryConsumeTerminator(out var terminatorToken);
        return FreestandingMacroMemberDeclaration(
            attributeLists,
            modifiers,
            invocation.Name,
            invocation.ExclamationToken,
            invocation.Carrier,
            terminatorToken);
    }

    public FreestandingMacroDeclarationSyntax ParseDeclaration(
        SyntaxList attributeLists,
        SyntaxList modifiers)
    {
        var name = ParseMacroName();
        ConsumeTokenOrMissing(SyntaxKind.ExclamationToken, out var exclamationToken);

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = MissingToken(SyntaxKind.IdentifierToken);
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.IdentifierExpected,
                GetEndOfLastToken()));
        }

        ParameterListSyntax? parameterList = null;
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
            parameterList = new StatementSyntaxParser(this).ParseParameterList();

        MacroTokenTreeSyntax tokenTree;
        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            tokenTree = ParseTokenTree();
        }
        else
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetSpanOfLastToken(),
                "{"));
            tokenTree = MacroTokenTree(
                MissingToken(SyntaxKind.OpenBraceToken),
                MissingToken(SyntaxKind.MacroBodyToken),
                MissingToken(SyntaxKind.CloseBraceToken));
        }

        TryConsumeTerminator(out var terminatorToken);
        var carrier = DeclarationMacroCarrier(identifier, parameterList, tokenTree);
        return FreestandingMacroDeclaration(
            attributeLists,
            modifiers,
            name,
            exclamationToken,
            carrier,
            terminatorToken);
    }

    private InvocationParts ParseInvocation(bool allowExpressionHeader)
    {
        var name = ParseMacroName();
        ConsumeTokenOrMissing(SyntaxKind.ExclamationToken, out var exclamationToken);
        ArgumentListSyntax? argumentList = null;

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            argumentList = new ExpressionSyntaxParser(this).ParseArgumentListSyntax(
                allowLegacyNamedArgumentEquals: false);
        }

        ExpressionSyntax? expression = null;
        if (argumentList is null &&
            allowExpressionHeader &&
            !HasLineBreakBeforePeekToken() &&
            !PeekToken().IsKind(SyntaxKind.OpenBraceToken) &&
            !IsOperatorLeadingHeader(PeekToken()))
        {
            expression = new ExpressionSyntaxParser(this, stopOnOpenBrace: true).ParseExpression();
        }

        var tokenTree = PeekToken().IsKind(SyntaxKind.OpenBraceToken) &&
            !HasLineBreakBeforePeekToken()
            ? ParseTokenTree()
            : null;

        if (argumentList is null && expression is null && tokenTree is null)
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetSpanOfLastToken(),
                "( or {"));
        }

        MacroCarrierSyntax carrier = argumentList switch
        {
            not null => ParenthesizedMacroCarrier(argumentList, tokenTree),
            null when expression is not null => ExpressionHeaderMacroCarrier(expression, tokenTree),
            _ => TokenTreeMacroCarrier(tokenTree!),
        };

        return new InvocationParts(name, exclamationToken, carrier);
    }

    private bool CanStartMacroName()
        => CanTokenBeIdentifier(PeekToken()) ||
            SyntaxFacts.IsKeywordKind(PeekToken().Kind) &&
            PeekToken(1).IsKind(SyntaxKind.ExclamationToken);

    private NameSyntax ParseMacroName()
    {
        if (SyntaxFacts.IsKeywordKind(PeekToken().Kind) &&
            PeekToken(1).IsKind(SyntaxKind.ExclamationToken))
        {
            var keyword = ToIdentifierToken(ReadToken());
            UpdateLastToken(keyword);
            return IdentifierName(keyword);
        }

        return new NameSyntaxParser(this).ParseName();
    }

    private static bool IsOperatorLeadingHeader(SyntaxToken token)
        => SyntaxFacts.IsUnaryOperatorToken(token.Kind) ||
            token.IsKind(SyntaxKind.DotToken);

    private MacroTokenTreeSyntax ParseTokenTree()
    {
        var openBraceToken = ReadToken();
        var bodyToken = ReadMacroBodyToken(out var isTerminated);
        var closeBraceToken = isTerminated
            ? ReadToken()
            : MissingToken(SyntaxKind.CloseBraceToken);

        if (!isTerminated)
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetSpanOfLastToken(),
                SyntaxFacts.GetSyntaxTokenText(SyntaxKind.CloseBraceToken) ?? "}"));
        }

        return MacroTokenTree(openBraceToken, bodyToken, closeBraceToken);
    }

    private readonly record struct InvocationParts(
        NameSyntax Name,
        SyntaxToken ExclamationToken,
        MacroCarrierSyntax Carrier);
}
