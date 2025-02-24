namespace Raven.CodeAnalysis.Syntax;

public partial class VariableDeclarationSyntax : SyntaxNode
{
    public partial SyntaxToken LetOrVarKeyword { get; }
    public partial SeparatedSyntaxList<VariableDeclaratorSyntax> Declarators { get; }

    internal VariableDeclarationSyntax(
        InternalSyntax.VariableDeclarationSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public VariableDeclarationSyntax(SyntaxToken letOrVarKeyword, SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
          : this(
                new InternalSyntax.VariableDeclarationSyntax(letOrVarKeyword.Green, declarators.Green), null)
    {

    }

    public VariableDeclarationSyntax(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        : this(SyntaxFactory.LetKeyword, declarators)
    {

    }
}

public static partial class SyntaxFactory
{
    public static VariableDeclarationSyntax VariableDeclaration(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        => VariableDeclaration(LetKeyword, declarators);

    public static VariableDeclarationSyntax VariableDeclaration(SyntaxToken letOrVarKeyword, SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        => new VariableDeclarationSyntax(letOrVarKeyword, declarators);
}