namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class TypeParameterListSyntax : SyntaxNode
{
    public TypeParameterListSyntax(
        SyntaxToken openParenToken,
        SeparatedSyntaxList parameters,
        SyntaxToken closeParentToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      openParenToken,
                      parameters,
                      closeParentToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.TypeParameterListSyntax(this, parent, position);
    }
}