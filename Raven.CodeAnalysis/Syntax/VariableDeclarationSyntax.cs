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

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitVariableDeclaration(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitVariableDeclaration(this);
    }
}

public static partial class SyntaxFactory
{
    public static VariableDeclarationSyntax VariableDeclaration(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        => new VariableDeclarationSyntax(declarators);
}