namespace Raven.CodeAnalysis.Syntax;

public abstract partial class TypeSyntax : SyntaxNode
{
    protected TypeSyntax(GreenNode greenNode, SyntaxNode parent, int position) : base(greenNode, parent, position)
    {
    }
}

public partial class NameSyntax : TypeSyntax
{
    internal NameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public NameSyntax(SyntaxToken nameToken)
        : base(new InternalSyntax.NameSyntax(nameToken.Green), (SyntaxNode)null, 0)
    {

    }

    public partial SyntaxToken NameToken { get; }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitName(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitName(this);
    }
}

public static partial class SyntaxFactory
{
    public static TypeSyntax ParseTypeName(string name)
        => new NameSyntax(IdentifierToken(name));
}