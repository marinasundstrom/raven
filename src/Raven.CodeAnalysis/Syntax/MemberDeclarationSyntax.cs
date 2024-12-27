namespace Raven.CodeAnalysis.Syntax;

public abstract class MemberDeclarationSyntax : SyntaxNode
{
    internal MemberDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public static implicit operator MemberDeclarationSyntax(StatementSyntax statement) => SyntaxFactory.GlobalStatement(statement);
}