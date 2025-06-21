namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static SyntaxFactory;

internal class TypeDeclarationParser : SyntaxParser
{
    public TypeDeclarationParser(ParseContext context) : base(context)
    {

    }

    internal BaseTypeDeclarationSyntax Parse()
    {
        var structOrClassKeyword = ReadToken();

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

        return null; //StructOrTypeDeclaration(structOrClassKeyword, identifier, openBraceToken, List(parameterList), closeBraceToken, terminatorToken);
    }

    private GreenNode ParseMember()
    {
        var identifier = ReadToken();

        return null;  //StructOrTypeMemberDeclaration(identifier);
    }
}