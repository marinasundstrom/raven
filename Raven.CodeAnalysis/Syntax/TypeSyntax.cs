namespace Raven.CodeAnalysis.Syntax;

public abstract partial class TypeSyntax : SyntaxNode
{
    protected TypeSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    protected TypeSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}

public partial class NameSyntax : TypeSyntax
{
    internal NameSyntax(GreenNode greenNode, SyntaxNode parent)
        : base(greenNode, parent)
    {
    }

    public NameSyntax(SyntaxToken nameToken)
    : base(new InternalSyntax.NameSyntax(nameToken.Green), (SyntaxNode)null)
    {

    }

    public partial SyntaxToken NameToken { get; }
}

public static partial class SyntaxFactory
{
    public static TypeSyntax ParseTypeName(string name)
        => new NameSyntax(IdentifierToken(name));
}