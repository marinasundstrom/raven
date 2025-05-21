namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NameParser : SyntaxParser
{
    public NameParser(SyntaxParser parent) : base(parent)
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

    private IdentifierNameSyntax ParseSimpleName()
    {
        var token = ReadToken();
        return IdentifierName(token);
    }
}
