namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static SyntaxFactory;

internal static class AttributeDeclarationParser
{
    public static SyntaxList ParseAttributeLists(SyntaxParser parser)
    {
        if (!parser.PeekToken().IsKind(SyntaxKind.OpenBracketToken))
            return SyntaxList.Empty;

        var attributeLists = new List<GreenNode>();

        while (parser.PeekToken().IsKind(SyntaxKind.OpenBracketToken))
        {
            attributeLists.Add(ParseAttributeList(parser));
        }

        return List(attributeLists);
    }

    private static AttributeListSyntax ParseAttributeList(SyntaxParser parser)
    {
        var openBracket = parser.ReadToken();

        var attributes = new List<GreenNode>();
        while (true)
        {
            attributes.Add(ParseAttribute(parser));

            var separator = parser.PeekToken();
            if (!separator.IsKind(SyntaxKind.CommaToken))
                break;

            parser.ReadToken();
            attributes.Add(separator);
        }

        parser.ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracket);

        return AttributeList(openBracket, List(attributes), closeBracket);
    }

    private static AttributeSyntax ParseAttribute(SyntaxParser parser)
    {
        var name = new NameSyntaxParser(parser).ParseName();

        ArgumentListSyntax? argumentList = null;
        if (parser.PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            argumentList = new ExpressionSyntaxParser(parser).ParseArgumentListSyntax();
        }

        return Attribute(name, argumentList);
    }
}
