namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class VariableDeclarationSyntax : SyntaxNode
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