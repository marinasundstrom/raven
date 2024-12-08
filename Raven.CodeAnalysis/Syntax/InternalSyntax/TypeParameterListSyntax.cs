namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class TypeParameterListSyntax : SyntaxNode
{
    public TypeParameterListSyntax(
        SyntaxToken openParenToken,
        SeparatedSyntaxList parameters,
        SyntaxToken closeParentToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      openParenToken,
                      parameters,
                      closeParentToken
              ],
              openParenToken.FullWidth + (parameters?.FullWidth ?? 0) + closeParentToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.TypeParameterListSyntax(this, parent);
    }
}
