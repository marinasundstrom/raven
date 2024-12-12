namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class TypeParameterListSyntax : SyntaxNode
{
    public TypeParameterListSyntax(
        SyntaxToken openParenToken,
        SeparatedSyntaxList parameters,
        SyntaxToken closeParentToken)
        : base(
              SyntaxKind.Block,
              [
                      openParenToken,
                      parameters,
                      closeParentToken
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ParameterListSyntax(this, parent, position);
    }
}