namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class VariableDeclarationSyntax : SyntaxNode
{
    public VariableDeclarationSyntax(
        SyntaxToken letKeyword,
        SeparatedSyntaxList variableDeclarators)
        : base(
              SyntaxKind.VariableDeclaration,
              [
                      letKeyword,
                      variableDeclarators
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.VariableDeclarationSyntax(this, parent, position);
    }
}