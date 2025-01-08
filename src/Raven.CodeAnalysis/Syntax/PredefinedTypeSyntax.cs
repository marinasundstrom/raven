namespace Raven.CodeAnalysis.Syntax;

public partial class PredefinedTypeSyntax : TypeSyntax
{
    public partial SyntaxToken Keyword { get; }

    internal PredefinedTypeSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent,
        int position)
        : base(greenNode, parent, position)
    {
    }

    public PredefinedTypeSyntax(SyntaxToken keyword)
      : this(
            new InternalSyntax.PredefinedTypeSyntax(keyword.Green), null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static PredefinedTypeSyntax PredefinedType(SyntaxToken keyword)
        => new PredefinedTypeSyntax(keyword);
}