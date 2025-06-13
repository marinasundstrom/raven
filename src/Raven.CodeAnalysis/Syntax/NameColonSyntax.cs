namespace Raven.CodeAnalysis.Syntax;

public partial class NameColonSyntax : SyntaxNode
{
    public partial IdentifierNameSyntax Name { get; }

    //public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken ColonTon { get; }

    internal NameColonSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public NameColonSyntax(
        IdentifierNameSyntax name,
        //ExpressionSyntax expression,
        SyntaxToken colonToken)
        : this(new InternalSyntax.NameColonSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, /* (InternalSyntax.ExpressionSyntax)expression.Green)*/ colonToken.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static NameColonSyntax NameColon(IdentifierNameSyntax name, SyntaxToken colonToken)
        => new NameColonSyntax(name, colonToken);
}