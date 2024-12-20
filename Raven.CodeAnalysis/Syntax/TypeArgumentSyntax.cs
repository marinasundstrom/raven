namespace Raven.CodeAnalysis.Syntax;

public partial class TypeArgumentSyntax : SyntaxNode
{
    public partial TypeSyntax Type { get; }

    internal TypeArgumentSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public TypeArgumentSyntax(
        TypeSyntax type)
        : this(new InternalSyntax.TypeArgumentSyntax((InternalSyntax.TypeSyntax)type.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArgumentSyntax TypeArgumentSyntax(TypeSyntax type)
        => new ArgumentSyntax(type);
}