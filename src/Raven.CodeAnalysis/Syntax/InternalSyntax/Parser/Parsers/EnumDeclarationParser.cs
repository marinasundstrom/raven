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

        if (!ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier))
        {

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
        var identifier = ReadToken();

        return EnumMemberDeclaration(SyntaxList.Empty, identifier, null);
    }
}