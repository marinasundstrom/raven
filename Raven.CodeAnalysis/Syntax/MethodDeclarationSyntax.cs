namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : SyntaxNode
{
    public partial SyntaxToken ReturnType { get; }
    public partial SyntaxToken Identifier { get; }
    public partial SeparatedSyntaxList Parameters { get; }
    public partial SyntaxList Body { get; }

    public MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    // Additional properties or methods specific to MethodDeclaration
}