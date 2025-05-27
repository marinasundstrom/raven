namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class TypeAnnotationSyntaxParser : SyntaxParser
{
    public TypeAnnotationSyntaxParser(ParseContext parent) : base(parent)
    {
    }

    public TypeAnnotationSyntax? ParseTypeAnnotation()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return SyntaxFactory.TypeAnnotation(colonToken, type);
        }

        return null;
    }

    public ReturnTypeAnnotationSyntax? ParseReturnTypeAnnotation()
    {
        if (ConsumeToken(SyntaxKind.ArrowToken, out var arrowToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return SyntaxFactory.ReturnTypeAnnotation(arrowToken, type);
        }

        return null;
    }
}