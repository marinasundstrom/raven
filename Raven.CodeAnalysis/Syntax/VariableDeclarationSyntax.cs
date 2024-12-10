namespace Raven.CodeAnalysis.Syntax;

public partial class VariableDeclarationSyntax : SyntaxNode
{
    public partial SyntaxToken LetKeyword { get; }
    public partial SeparatedSyntaxList<VariableDeclaratorSyntax> Declarators { get; }

    public VariableDeclarationSyntax(
        InternalSyntax.VariableDeclarationSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public VariableDeclarationSyntax(SyntaxToken letKeyword, SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
          : this(
                new InternalSyntax.VariableDeclarationSyntax(letKeyword.Green, declarators.Green), null)
    {

    }

    public VariableDeclarationSyntax(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        : this(SyntaxFactory.LetKeyword, declarators)
    {

    }

    // Additional properties or methods specific to BlockSyntax can be added here.
}

public static partial class SyntaxFactory
{
    public static VariableDeclarationSyntax VariableDeclaration(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        => new VariableDeclarationSyntax(declarators);
}