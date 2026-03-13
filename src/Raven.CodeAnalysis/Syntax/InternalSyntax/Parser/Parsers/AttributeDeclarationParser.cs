namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static SyntaxFactory;

using SyntaxFacts = Raven.CodeAnalysis.Syntax.SyntaxFacts;

internal static class AttributeDeclarationParser
{
    public static SyntaxList ParseAttributeLists(SyntaxParser parser)
    {
        if (!IsAttributeListStart(parser))
            return SyntaxList.Empty;

        var attributeLists = new List<GreenNode>();

        while (IsAttributeListStart(parser))
        {
            attributeLists.Add(ParseAttributeList(parser));
        }

        return List(attributeLists);
    }

    public static AttributeListSyntax ParseAttributeList(SyntaxParser parser)
    {
        SyntaxToken? hashToken = null;
        if (parser.PeekToken().IsKind(SyntaxKind.HashToken))
            hashToken = parser.ReadToken();

        var openBracket = parser.ReadToken();

        AttributeTargetSpecifierSyntax? target = null;
        if ((SyntaxFacts.CanBeIdentifier(parser.PeekToken().Kind) || SyntaxFacts.IsReservedWordKind(parser.PeekToken().Kind))
            && parser.PeekToken(1).IsKind(SyntaxKind.ColonToken))
        {
            var identifier = parser.ReadToken();
            parser.ConsumeTokenOrMissing(SyntaxKind.ColonToken, out var colonToken);
            target = AttributeTargetSpecifier(identifier, colonToken);
        }

        var attributes = new List<GreenNode>();
        while (true)
        {
            attributes.Add(ParseAttribute(parser, hashToken));
            hashToken = null;

            var separator = parser.PeekToken();
            if (!separator.IsKind(SyntaxKind.CommaToken))
                break;

            parser.ReadToken();
            attributes.Add(separator);
        }

        parser.ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracket);

        return AttributeList(openBracket, target, List(attributes), closeBracket);
    }

    private static AttributeSyntax ParseAttribute(SyntaxParser parser, SyntaxToken? hashToken = null)
    {
        var name = new NameSyntaxParser(parser).ParseName();

        ArgumentListSyntax? argumentList = null;
        if (parser.PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            argumentList = new ExpressionSyntaxParser(parser).ParseArgumentListSyntax(allowLegacyNamedArgumentEquals: false);
        }

        return Attribute(hashToken ?? Token(SyntaxKind.None), name, argumentList);
    }

    internal static bool IsAttributeListStart(SyntaxParser parser)
        => parser.PeekToken().IsKind(SyntaxKind.OpenBracketToken)
            || (parser.PeekToken().IsKind(SyntaxKind.HashToken) && parser.PeekToken(1).IsKind(SyntaxKind.OpenBracketToken));
}
