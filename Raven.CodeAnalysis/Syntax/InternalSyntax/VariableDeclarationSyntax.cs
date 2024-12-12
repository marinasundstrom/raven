namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class VariableDeclarationSyntax : SyntaxNode
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
}