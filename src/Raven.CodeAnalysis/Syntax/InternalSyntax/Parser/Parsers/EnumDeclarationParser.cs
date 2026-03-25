namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static SyntaxFactory;

internal class EnumDeclarationParser : SyntaxParser
{
    public EnumDeclarationParser(ParseContext context) : base(context)
    {

    }

    internal EnumDeclarationSyntax Parse()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseModifiers();

        var enumKeyword = ReadToken();

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        BaseListSyntax? baseList = ParseBaseList();

        List<GreenNode> parameterList = new List<GreenNode>();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);
        var explicitSeparatorKind = SyntaxKind.None;

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBraceToken))
                break;

            var member = ParseMember();

            parameterList.Add(member);

            if (TryConsumeMemberSeparator(out var separatorToken, out var explicitSeparatorKindForToken))
            {
                if (explicitSeparatorKindForToken != SyntaxKind.None)
                {
                    if (explicitSeparatorKind == SyntaxKind.None)
                    {
                        explicitSeparatorKind = explicitSeparatorKindForToken;
                    }
                    else if (explicitSeparatorKind != explicitSeparatorKindForToken)
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                SyntaxFacts.GetSyntaxTokenText(explicitSeparatorKind) ?? explicitSeparatorKind.ToString()));
                    }
                }

                parameterList.Add(separatorToken);
            }
            else if (!PeekToken().IsKind(SyntaxKind.CloseBraceToken) &&
                     !PeekToken().IsKind(SyntaxKind.EndOfFileToken))
            {
                parameterList.Add(MissingToken(SyntaxKind.CommaToken));
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetSpanOfPeekedToken(),
                        ","));
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        TryConsumeTerminator(out var terminatorToken);

        return EnumDeclaration(attributeLists, modifiers, enumKeyword, identifier, baseList, openBraceToken, List(parameterList), closeBraceToken, terminatorToken);
    }

    private BaseListSyntax? ParseBaseList()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            var types = new List<GreenNode>();
            while (true)
            {
                var type = new NameSyntaxParser(this).ParseTypeName();
                types.Add(SimpleBaseType(type));

                var commaToken = PeekToken();
                if (commaToken.IsKind(SyntaxKind.CommaToken))
                {
                    ReadToken();
                    types.Add(commaToken);
                }
                else
                {
                    break;
                }
            }

            return BaseList(colonToken, List(types));
        }

        return null;
    }

    private GreenNode ParseMember()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        EqualsValueClauseSyntax? initializer = null;
        if (IsNextToken(SyntaxKind.EqualsToken, out _))
            initializer = new EqualsValueClauseSyntaxParser(this).Parse();

        return EnumMemberDeclaration(attributeLists, SyntaxList.Empty, identifier, initializer);
    }

    private bool TryConsumeMemberSeparator(out SyntaxToken separatorToken, out SyntaxKind explicitSeparatorKind)
    {
        var current = PeekToken();

        if (current.IsKind(SyntaxKind.CommaToken) || current.IsKind(SyntaxKind.SemicolonToken))
        {
            separatorToken = ReadToken();
            explicitSeparatorKind = separatorToken.Kind;
            return true;
        }

        if (HasLineBreakBeforePeekToken())
        {
            separatorToken = Token(SyntaxKind.None);
            explicitSeparatorKind = SyntaxKind.None;
            return true;
        }

        if (current.IsKind(SyntaxKind.CloseBraceToken) || current.IsKind(SyntaxKind.EndOfFileToken))
        {
            separatorToken = Token(SyntaxKind.None);
            explicitSeparatorKind = SyntaxKind.None;
            return false;
        }

        separatorToken = Token(SyntaxKind.None);
        explicitSeparatorKind = SyntaxKind.None;
        return false;
    }

    private SyntaxList ParseModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.PublicKeyword or
                     SyntaxKind.PrivateKeyword or
                     SyntaxKind.InternalKeyword or
                     SyntaxKind.ProtectedKeyword or
                     SyntaxKind.FilescopeKeyword or
                     SyntaxKind.StaticKeyword or
                     SyntaxKind.AbstractKeyword or
                     SyntaxKind.FinalKeyword or
                     SyntaxKind.SealedKeyword or
                     SyntaxKind.PartialKeyword or
                     SyntaxKind.VirtualKeyword or
                     SyntaxKind.RecordKeyword or
                     SyntaxKind.OpenKeyword or
                     SyntaxKind.OverrideKeyword)
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
}
