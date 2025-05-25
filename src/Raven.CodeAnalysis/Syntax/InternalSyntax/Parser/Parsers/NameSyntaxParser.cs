namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

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

    public IdentifierNameSyntax ParseSimpleName()
    {
        var token = ReadToken();
        return IdentifierName(token);
    }

    private bool IsPredefinedTypeKeyword(SyntaxToken token)
    {
        switch (token.Kind)
        {
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.IntKeyword:
                return true;
        }

        return false;
    }
}
