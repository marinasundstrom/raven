namespace Raven.CodeAnalysis.Syntax;

public abstract partial class BasePropertyDeclarationSyntax : MemberDeclarationSyntax
{
    internal BasePropertyDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxToken Identifier { get; }
    public abstract ArrowTypeClauseSyntax Type { get; }
    public abstract AccessorListSyntax? AccessorList { get; }
}
