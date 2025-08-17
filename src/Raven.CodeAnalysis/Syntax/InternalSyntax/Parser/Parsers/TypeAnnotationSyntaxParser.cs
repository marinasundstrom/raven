namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class TypeAnnotationClauseSyntaxParser : SyntaxParser
{
    public TypeAnnotationClauseSyntaxParser(ParseContext parent) : base(parent)
    {
    }

    public TypeAnnotationClauseSyntax? ParseTypeAnnotation()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return SyntaxFactory.TypeAnnotationClause(colonToken, type);
        }

        return null;
    }

    public ArrowTypeClauseSyntax? ParseReturnTypeAnnotation()
    {
        if (ConsumeToken(SyntaxKind.ArrowToken, out var arrowToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return SyntaxFactory.ArrowTypeClause(arrowToken, type);
        }

        return null;
    }
}