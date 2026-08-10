namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal sealed class MacroInvocationSyntaxParser : SyntaxParser
{
    public MacroInvocationSyntaxParser(ParseContext parent) : base(parent)
    {
    }

    public bool IsBangInvocationStart()
    {
        if (!CanTokenBeIdentifier(PeekToken()))
            return false;

        var checkpoint = CreateCheckpoint("bang-macro-lookahead");
        var name = new NameSyntaxParser(this).ParseName();
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

        isStart = !HasLineBreakBeforePeekToken() &&
            PeekToken().IsKind(SyntaxKind.OpenBraceToken);
        checkpoint.Rewind();
        return isStart;
    }

    public BangMacroExpressionSyntax ParseExpression()
    {
        var invocation = ParseInvocation();
        return BangMacroExpression(
            invocation.Name,
            invocation.ExclamationToken,
            invocation.ArgumentList,
            invocation.TokenTree);
    }

    public FreestandingMacroMemberDeclarationSyntax ParseMember(
        SyntaxList attributeLists,
        SyntaxList modifiers)
    {
        var invocation = ParseInvocation();
        TryConsumeTerminator(out var terminatorToken);
        return FreestandingMacroMemberDeclaration(
            attributeLists,
            modifiers,
            invocation.Name,
            invocation.ExclamationToken,
            invocation.ArgumentList,
            invocation.TokenTree,
            terminatorToken);
    }

    private InvocationParts ParseInvocation()
    {
        var name = new NameSyntaxParser(this).ParseName();
        ConsumeTokenOrMissing(SyntaxKind.ExclamationToken, out var exclamationToken);
        var argumentList = CreateMissingArgumentList();

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            argumentList = new ExpressionSyntaxParser(this).ParseArgumentListSyntax(
                allowLegacyNamedArgumentEquals: false);
        }

        var tokenTree = PeekToken().IsKind(SyntaxKind.OpenBraceToken)
            ? ParseTokenTree()
            : null;

        if (argumentList.OpenParenToken.IsMissing && tokenTree is null)
        {
            AddDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.CharacterExpected,
                GetSpanOfLastToken(),
                "( or {"));
        }

        return new InvocationParts(name, exclamationToken, argumentList, tokenTree);
    }

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

    private static ArgumentListSyntax CreateMissingArgumentList()
        => ArgumentList(
            MissingToken(SyntaxKind.OpenParenToken),
            List(Array.Empty<GreenNode>()),
            MissingToken(SyntaxKind.CloseParenToken));

    private readonly record struct InvocationParts(
        NameSyntax Name,
        SyntaxToken ExclamationToken,
        ArgumentListSyntax ArgumentList,
        MacroTokenTreeSyntax? TokenTree);
}
