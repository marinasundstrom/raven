namespace Raven.CodeAnalysis.Syntax;

public partial class AccessorListSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList<AccessorDeclarationSyntax> Accessors { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    internal AccessorListSyntax(
        InternalSyntax.AccessorListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public AccessorListSyntax(SyntaxToken openBraceToken, SyntaxList<AccessorDeclarationSyntax> accessors, SyntaxToken closeBraceToken)
          : this(
                new InternalSyntax.AccessorListSyntax(openBraceToken.Green, accessors.Green, closeBraceToken.Green), null)
    {

    }

    public AccessorListSyntax(SyntaxList<AccessorDeclarationSyntax> accessors)
        : this(SyntaxFactory.OpenBraceToken, accessors, SyntaxFactory.CloseBraceToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static AccessorListSyntax AccessorList()
    => new AccessorListSyntax(SyntaxList<AccessorDeclarationSyntax>.Empty);

    public static AccessorListSyntax AccessorList(SyntaxList<AccessorDeclarationSyntax> accessors)
        => new AccessorListSyntax(accessors);

    public static AccessorListSyntax AccessorList(SyntaxToken openBraceToken, SyntaxList<AccessorDeclarationSyntax> accessors, SyntaxToken closeBraceToken)
        => new AccessorListSyntax(openBraceToken, accessors, closeBraceToken);
}