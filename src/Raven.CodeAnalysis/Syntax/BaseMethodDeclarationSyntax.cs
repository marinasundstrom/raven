namespace Raven.CodeAnalysis.Syntax;

public abstract partial class BaseMethodDeclarationSyntax : MemberDeclarationSyntax
{
    internal BaseMethodDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract ParameterListSyntax ParameterList { get; }
    public abstract BlockSyntax? Body { get; }
    public abstract ArrowExpressionClauseSyntax? ExpressionBody { get; }
}
