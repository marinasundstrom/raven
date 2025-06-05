namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NameSyntaxParser : SyntaxParser
{
    public NameSyntaxParser(ParseContext parent) : base(parent)
    {

    }


    public NameSyntax ParseName()
    {
        NameSyntax left = ParseSimpleName();

        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            left = QualifiedName(left, dotToken, ParseSimpleName());
        }

        return left;
    }

    public TypeSyntax ParseTypeName()
    {
        var name = ParseNameCore();

        SyntaxList types = SyntaxList.Empty;

        bool isUnion = false;

        if (IsNextToken(SyntaxKind.BarToken))
        {
            isUnion = true;
            types = types.Add(name);
        }

        while (ConsumeToken(SyntaxKind.BarToken, out var barToken))
        {
            types = types.Add(barToken);
            types = types.Add(ParseNameCore());
        }

        if (isUnion)
        {
            return UnionType(types);
        }

        return name;
    }

    private TypeSyntax ParseNameCore()
    {
        var peek = PeekToken();
        if (IsPredefinedTypeKeyword(peek))
        {
            ReadToken();

            return PredefinedType(peek);
        }

        TypeSyntax left = ParseSimpleName();

        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            left = QualifiedName((NameSyntax)left, dotToken, ParseSimpleName());
        }

        if (ConsumeToken(SyntaxKind.QuestionToken, out var questionToken))
        {
            left = NullableType(left, questionToken);
        }

        return left;
    }

    public SimpleNameSyntax ParseSimpleName()
    {
        var name = ReadToken();

        if (name.IsKind(SyntaxKind.IdentifierToken) &&
            PeekToken().IsKind(SyntaxKind.LessThanToken) &&
            LooksLikeTypeArgumentList())
        {
            var typeArgList = ParseTypeArgumentList();
            return GenericName(name, typeArgList);
        }

        return IdentifierName(name);
    }

    private TypeArgumentListSyntax ParseTypeArgumentList()
    {
        var lessThanToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.GreaterThanToken))
                break;

            var typeName = new NameSyntaxParser(this).ParseTypeName();
            if (typeName is null)
                break;

            argumentList.Add(TypeArgument(typeName));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                argumentList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.GreaterThanToken, out var greaterThanToken);

        return TypeArgumentList(lessThanToken, List(argumentList.ToArray()), greaterThanToken);
    }

    private bool LooksLikeTypeArgumentList()
    {
        int depth = 0;
        int i = 0;

        while (true)
        {
            var token = PeekToken(i++);

            if (token.IsKind(SyntaxKind.LessThanToken))
            {
                depth++;
            }
            else if (token.IsKind(SyntaxKind.GreaterThanToken))
            {
                depth--;
                if (depth == 0)
                    return true;
            }
            else if (token.IsKind(SyntaxKind.EndOfFileToken) ||
                     token.IsKind(SyntaxKind.SemicolonToken))
            {
                return false;
            }
            else if (token.IsMissing)
            {
                return false;
            }
            else if (depth == 0)
            {
                return false; // Not inside <...>
            }

            if (i > 20)
                return false; // bail out
        }
    }

    private bool IsPredefinedTypeKeyword(SyntaxToken token)
    {
        switch (token.Kind)
        {
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.IntKeyword:
            case SyntaxKind.VoidKeyword:
                return true;
        }

        return false;
    }
}
