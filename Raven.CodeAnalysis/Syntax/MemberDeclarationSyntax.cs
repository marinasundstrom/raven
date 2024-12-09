namespace Raven.CodeAnalysis.Syntax;

public abstract class MemberDeclarationSyntax : SyntaxNode
{
    public MemberDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent) : base(greenNode, parent)
    {
    }

    public MemberDeclarationSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }

    public static implicit operator MemberDeclarationSyntax(StatementSyntax statement) => SyntaxFactory.GlobalStatement(statement);
}