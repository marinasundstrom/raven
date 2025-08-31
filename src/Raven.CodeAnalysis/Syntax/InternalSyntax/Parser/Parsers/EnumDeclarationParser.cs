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

        List<GreenNode> parameterList = new List<GreenNode>();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBraceToken))
                break;

            var member = ParseMember();

            parameterList.Add(member);

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                parameterList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        TryConsumeTerminator(out var terminatorToken);

        return EnumDeclaration(SyntaxList.Empty, enumKeyword, identifier, openBraceToken, List(parameterList), closeBraceToken, terminatorToken);
    }

    private GreenNode ParseMember()
    {
        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        return EnumMemberDeclaration(SyntaxList.Empty, identifier, null);
    }
}